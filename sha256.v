/*
	clock = clock signal
	reset = clear input
	load_enable = start loading input
	input_complete = input complete
	input_data = 8 bits of input
	hased_data = 16 bits of hashed output
	NOTE: Data is inputted left to right and the hash is outputted left to right
*/
module SHA256 #() (
	input logic clock, reset, load_enable, input_complete,
	input logic[7:0] input_data,
	output logic[15:0] hashed_data
);
	// Internal signals
	wire [511:0] 	padded_message_block;
	wire 		 	padding_done;
	wire [2047:0] 	schedule_data;
	wire [255:0] 	final_hash;
	wire 			schedule_ready;
	wire			compression_done;
	wire			hash_ready;

	// Instantiate input loader and padder
	SHA256_input loader_and_padder (
		.clock(clock),
		.reset(reset),
		.load_enable(load_enable),
		.input_complete(input_complete),
		.input_data(input_data),
		.padded_block(padded_message_block),
		.padding_done(padding_done)
	);

	// Convert 512 bit block to 2048 bit messageschedule
	SHA256_Message_Scheduler message_scheduler(
		.clock(clock), .reset(reset), .start(padding_done),
		.padded_block(padded_message_block),
		.schedule_data(schedule_data),
		.schedule_ready(schedule_ready)
	);


	// Instantiate compression engine
	SHA256_Compression_Engine compression_inst (
		.clock(clock),
		.reset(reset),
		.start(schedule_ready),
		.schedule_data(schedule_data),
		.final_hash(final_hash),
		.done(compression_done)
	);

	// Handle the output
	SHA256_Output_Handler output_handler (
		.clock(clock), .reset(reset),
		.hash_ready(compression_done),
		.final_hash(final_hash),
		.hashed_data(hashed_data),
		.done(hash_ready)
	);
endmodule


// Input module loads in input from user 8 bits (1 byte) at a time
//	This module also pads the imput to 512
// 	User input can be a binary string from size 0 to 512
module SHA256_input	(
	input logic				clock, reset, load_enable, input_complete,
	input logic	[7:0]		input_data,
	output logic [511:0] 	padded_block,
	output logic 			padding_done
);
	// Internal registers for data storage and counting
	reg [511:0] message;
	reg [5:0]	byte_count;
	reg [8:0]	length_bits;

	always @(posedge clock or posedge reset) begin
		if (reset) begin
			message 		<= 512'b0;
			byte_count 		<= 6'd0;
			length_bits		<= 9'd0;
			padding_done 	<= 1'b0;
		end else begin
			if (load_enable && !input_complete && byte_count < 63) begin
				message 	<= {message[503:0], input_data};
				byte_count 	<= byte_count + 1;
			end

			if (input_complete && !padding_done) begin
				length_bits = byte_count * 8;

				// Preform padding
				// 1. Append '10000000' (8'h0) after last byte of message
				if (byte_count == 0) begin
					message <= {8'h0, message[503:0]};
					byte_count <= 1;
				end else if (byte_count < 63) begin
					// Insert 0x80 at next position remaining space is zero-padded
					message <= {message[(byte_count*8)-1:0], 8'h80, {(512 - (byte_count*8) - 8){1'b0}}};
					byte_count <= 63;
				end

				// Insert length in last 8 bits
				message[7:0] <= length_bits[7:0];
				padded_block <= message;
				padding_done <= 1'b1;
			end
		end
	end
endmodule



// s0 = (w[i-15] rightrotate 7) xor (w[i-15] rightrotate 18) xor (w[i-15] righthift 3)
module scheduler_sigma0 (
	input wire [31:0] in_word,
	output wire [31:0] out_word
);
	assign out_word = ({in_word[6:0], in_word[31:7]} ^ {in_word[17:0], in_word[31:18]} ^ (in_word >> 3));
endmodule

// s1 = (w[i-2] rightrotate 17) xor (w[i-2] rightrotate 19) xor (w[i-2] righthift 10)
module scheduler_sigma1 (
	input wire [31:0] in_word,
	output wire [31:0] out_word
);
	assign out_word = ({in_word[16:0], in_word[31:17]} ^ {in_word[18:0], in_word[31:19]} ^ (in_word >> 10));
endmodule

// This message scheduler takes out 512 bit input and outputs a 2048 bit scheduler
module SHA256_Message_Scheduler (
	input wire				clock, reset, start,
	input wire [511:0]		padded_block,
	output reg [2047:0]	schedule_data,
	output reg			schedule_ready
);
	// Internal State Machine
	typedef enum logic [1:0] {
		IDLE = 2'b00;
		LOAD = 2'b01;
		CALC = 2'b10;
		DONE = 2'b11;
	} state_t;

	state_t current_state, next_state;

	// Word counter 0->63
	reg [6:0] counter;

	// Functions to get and set words in scheduled data 
	function automatic [31:0] get_word(
		input [2047:0] data, 
		input integer index
	);
		get_word = data[(2047 - (index*32)) : (2047 - (index*32) - 31)];
	endfunction

	function automatic [2047:0] set_word(
		input [2047:0] data, input integer index, input [31:0] word
	); 
		reg [2047:0] tmp;
		integer bitpos;
		begin
			tmp = data;
			bitpos = 2047 - (index*32);
			tmp[bitpos -: 32] = word;
			set_word = tmp;
		end
	endfunction

	// Registers to store intermediate words during calculation
	reg [31:0] w_tm2_reg, w_tm7_reg, w_tm15_reg, w_tm16_reg;
	reg [31:0] w_temp;

	// Sigma module outputs
	wire [31:0] sigma0_out, sigma1_out;

	// sigma module
	scheduler_sigma0 sigma0_inst (.in_word(w_tm15_reg), .out_word(sigma0_out));
	scheduler_sigma1 sigma1_inst (.in_word(w_tm2_reg), .out_word(sigma1_out));

	// Next state logic
	always @(*) begin
		next_state = current_state;
		case (current_state)
			IDLE: if (start) next_state = LOAD;
			LOAD: next_state = CALC;
			CALC: if (counter==63) next_state = DONE;
			DONE: next_state = DONE;
		endcase
	end

	// State operations
	always @(posedge clock or posedge reset) begin
		if (reset) begin
			schedule_data <= 2048'b0;
			schedule_ready <= 1'b0;
			counter <= 0;
			w_temp <= 32'h0;
		end else begin
			case (current_state)
				IDLE: begin
					schedule_ready <= 1'b0;
					counter <= 0;
				end

				LOAD: begin
					// Place padded_block into W[0..15] (first 512 bits)
					//		REMEMBER W[0] is bits 2048->2016 (most significant)
					schedule_data[2047:1536] <= padded_block;
					counter <= 16; // start calculating 
				end

				CALC: begin
					// Compute W[t] for t >= 16
                    // W[t] = σ1(W[t-2]) + W[t-7] + σ0(W[t-15]) + W[t-16]

					// get nessecary words
					w_tm2_reg  <= get_word(scheduled_data, counter-2);
                    w_tm7_reg  <= get_word(scheduled_data, counter-7);
                    w_tm15_reg <= get_word(scheduled_data, counter-15);
                    w_tm16_reg <= get_word(scheduled_data, counter-16);
					
					w_temp = sigma1_out + w_tm7_reg + sigma0_out + w_tm16_reg;
					scheduled_data <= set_word(scheduled_data, t, w_temp);
					
					if (counter < 63) begin
						counter <= counter + 1;
					end
				end

				DONE: begin
					// All W words computed
					schedule_ready <= 1'b1;
				end
			endcase
		end
	end
endmodule


/*
	There are 64, 32 bit constants.
	Each constant is the first 32 bits of the fractional parts
	of the cube roots of the first 64 primes (2-311).
	These constants are vital in the hashing algorithm.


*/
module SHA256_Constants (
	input wire clock, 
	input wire reset,
	output reg [31:0] output_constant
);

	// Internals
	reg [2047:0] constant_array;
	wire [2047:0] rotated_array = { constant_array[2015:0], constant_array[2047:2016] };
	assign output_constant = constant_array[2047:2016];

	always @(posedge clock) begin
		if (reset) begin
			constant_array <= {
				32'h428a2f98, 32'h71374491, 32'hb5c0fbcf, 32'he9b5dba5,
				32'h3956c25b, 32'h59f111f1, 32'h923f82a4, 32'hab1c5ed5,
				32'hd807aa98, 32'h12835b01, 32'h243185be, 32'h550c7dc3,
				32'h72be5d74, 32'h80deb1fe, 32'h9bdc06a7, 32'hc19bf174,
				32'he49b69c1, 32'hefbe4786, 32'h0fc19dc6, 32'h240ca1cc,
				32'h2de92c6f, 32'h4a7484aa, 32'h5cb0a9dc, 32'h76f988da,
				32'h983e5152, 32'ha831c66d, 32'hb00327c8, 32'hbf597fc7,
				32'hc6e00bf3, 32'hd5a79147, 32'h06ca6351, 32'h14292967,
				32'h27b70a85, 32'h2e1b2138, 32'h4d2c6dfc, 32'h53380d13,
				32'h650a7354, 32'h766a0abb, 32'h81c2c92e, 32'h92722c85,
				32'ha2bfe8a1, 32'ha81a664b, 32'hc24b8b70, 32'hc76c51a3,
				32'hd192e819, 32'hd6990624, 32'hf40e3585, 32'h106aa070,
				32'h19a4c116, 32'h1e376c08, 32'h2748774c, 32'h34b0bcb5,
				32'h391c0cb3, 32'h4ed8aa4a, 32'h5b9cca4f, 32'h682e6ff3,
				32'h748f82ee, 32'h78a5636f, 32'h84c87814, 32'h8cc70208,
				32'h90befffa, 32'ha4506ceb, 32'hbef9a3f7, 32'hc67178f2
			};
		end else begin 
			constant_array = rotated_array;
		end
	end
endmodule

// This is our compression / hashing function
//	The first input is the constant and weight words at out index i
//	The second input 8, 32 bit words that we will continuously hash
//	The output is the next input to this function (Originally, all words are our 
// 		first 8 prime constants
//	Computations:
//			hash1 = (e rightrotate 6) xor (e rightrotate 11) xor (e rightrotate 25)
//			hash0 = (a rightrotate 2) xor (a rightrotate 13) xor (a rightrotate 22)
//			choose = (e and f) xor ((!e) and g)
//			majority = (a and b) xor (a and c) xor (b and c)
//			word0 = h + hash1 + choose + constant + weight
//			word1 = hash0 + choose + constant + weight
//	Outputs:
//		a <= word0 + word1
//		b <= a
//		c <= b
//		d <= c
//		e <= d + word0
//		f <= e
//		g <= f
//		h <= g
module hash1 (
	input logic [31:0] e_in,
	output logic [31:0] hash1_out
);
	assign hash1_out = ({e_in[5:0], e_in[31:6]} ^ {e_in[10:0], e_in[31:11]} ^ {e_in[24:0], e_in[31:25]});
endmodule

module hash0 (
	input logic [31:0] a_in,
	output logic [31:0] hash0_out
);
	assign hash0_out = ({a_in[1:0], a_in[31:2]} ^ {a_in[12:0], a_in[31:13]} ^ {a_in[21:0], a_in[31:22]});
endmodule

module choose (
	input logic [31:0] e_in, f_in, g_in,
	output logic [31:0] choose_out
);
	assign choose_out = ((e_in & f_in) ^ ((~e_in) & g_in));
endmodule

module majority (
	input logic  [31:0] a_in, b_in, c_in,
	output logic [31:0] majority_out
);
	assign majority_out = (a_in & b_in) ^ (a_in & c_in) ^ (b_in & c_in);
endmodule


module Compression_Round (
	input logic [31:0] constant_i, weight_i,
	input logic [31:0] a_in, b_in, c_in, d_in, e_in, f_in, g_in, h_in,
	output logic [31:0] a_out, b_out, c_out, d_out, e_out, f_out, g_out, h_out
);
	// Internal wires
	wire [31:0] hash1_out, hash0_out;
	wire [31:0] choose_out, majority_out;

	// Sub-Module calls
	hash1 hash1_inst (
		.e_in(e_in),
		.hash1_out(hash1_out)
	);

	hash0 hash0_inst (
		.a_in(a_in),
		.hash0_out(hash0_out)
	);

	choose choose_inst (
		.e_in(e_in), .f_in(f_in), .g_in(g_in),
		.choose_out(choose_out)
	);

	majority majority_inst (
		.a_in(a_in), .b_in(b_in), .c_in(c_in),
		.majority_out(majority_out)
	);
	
	// Compute values
	wire [31:0] word0 = h_in + hash1_out + choose_out + constant_i + weight_i;
	wire [31:0] word1 = hash0_out + majority_out;
	
	// Assign outputs
	assign a_out = word0 + word1;
	assign b_out = a_in;
	assign c_out = b_in;
	assign d_out = c_in;
	assign e_out = d_in + word0;
	assign f_out = e_in;
	assign g_out = f_in;
	assign h_out = g_in;
endmodule

module SHA256_Compression_Engine (
	input logic 			clock, reset, start,
	input logic [2047:0] 	schedule_data,
	output logic [255:0] 	final_hash,
	output logic			done
);
	// Internal states, registers, counters
	reg [31:0] 	a, b, c, d, e, f, g, h;
	reg [5:0]	round_counter;
	wire [31:0]	constant_i;
	wire [31:0]	weight_i;
	wire [31:0]	a_out, b_out, c_out, d_out, e_out, f_out, g_out, h_out;

	// Constants block
	SHA256_Constants constants_inst (
		.clock(clock),
		.reset(reset),
		.output_constant(constant_i)
	);

	assign weight_i = schedule_data[((round_counter)*32)+31 -: 32];

	// Inital Hash Values
	localparam [255:0] H0 = {
		32'h6A09E667, 32'hBB67AE85, 32'h3C6EF372, 32'hA54FF53A,
		32'h510E527F, 32'h9B05688C, 32'h1F83D9AB, 32'h5BE0CD19
	};

	always @(posedge clock or posedge reset) begin
		if (reset) begin
			{a, b, c, d, e, f, g, h} <= H0;
			round_counter <= 0;
			done <= 0;
		end else if (start && !done) begin
			if (round_counter < 64) begin
				// Preform a round
				Compression_Round compression_round (
					.a(a), .b(b), .c(c), .d(d),
					.e(e), .f(f), g(g), .h(h),
					.constant_i(constant_i),
					.weight_i(weight_i),
					.a_out(a_out), .b_out(b_out), .c_out(c_out), .d_out(d_out),
					.e_out(e_out), .f_out(f_out), .g_out(g_out), .h_out(h_out)
				);

				// Update registers
				{a, b, c, d, e, f, g, h} <= {a_out, b_out, c_out, d_out, e_out, f_out, g_out, h_out};
				round_counter <= round_counter + 1;
			end else if (round_counter == 64) begin
				// add original hash values
				final_hash[255:224] <= a + H0[255:224];
                final_hash[223:192] <= b + H0[223:192];
                final_hash[191:160] <= c + H0[191:160];
                final_hash[159:128] <= d + H0[159:128];
                final_hash[127:96]  <= e + H0[127:96];
                final_hash[95:64]   <= f + H0[95:64];
                final_hash[63:32]   <= g + H0[63:32];
                final_hash[31:0]    <= h + H0[31:0];
				done <= 1;
			end
		end
	end
endmodule


// This module takes the final 256 bit hash and outputs 16 bits at a time 
//	once the compression is complete
module SHA256_Output_Handler (
	input logic				clock, reset,
	input logic				hash_ready,
	input logic [255:0] 	final_hash,
	output logic [15:0]		hashed_data,
	output logic				done
);

	// Internal
	reg [255:0] hash_buffer;
	reg [4:0]	output_counter;

	always @(posedge clock or posedge reset) begin
		if (reset) begin
			hashed_data <= 16'b0;
			hash_buffer <= 256'b0;
			output_counter <= 0;
			done <= 0;
		end else begin
			if (hash_ready && output_counter == 0) begin
				hash_buffer <= final_hash;
				output_counter <= 16; 	// 256 bits / 16 = 16 steps
				done <= 0;
			end else if (output_counter > 0) begin
				hashed_data <= hash_buffer[255:240];
				hash_buffer <= {hash_buffer[239:0], 16'b0};
				output_counter <= output_counter - 1;
				if (output_counter == 1) done <= 1; // All words have been sent out
			end
		end
	end
endmodule