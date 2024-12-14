/*
	clk = clock signal
	reset = clear input
	load_enable = start loading input
	input_complete = input complete
	input_data = 8 bits of input
	hased_data = 16 bits of hashed output
*/


module SHA256 #() (
	input logic clk, reset, load_enable, input_complete,
	input logic[7:0] input_data,
	output logic[15:0] hashed_data
);
	// PREPROCESSING THE DATA
	// STEP 1: Get input 1 bytes/8 bits/one character at a time
	reg [511:0] data_storage; // 512 bit input (We are only inputting 504 bits to allow for the additional 1 + padding
	reg [5:0] counter; // 6 bits are enough to count upto 64 (64 * 8 = 512)
	
	// NOTE: we need exactly 512 bits, therefore we will load in 63 times (63 * 8 = 504 bits)
	//		 so that we can add one and pad (part of SHA256 algorithm)
	always @(posedge clk or posedge reset) begin
		// If reset, clear the data_storage and counter
		if (reset) begin
			data_storage <= 512'b0;
			counter <= 0;
		end else if (load_enable && !input_complete && counter < 63) begin
			data_storage <= {data_storage[503:0], input_data}; // Shift what is currently in the register to the left by 8 and load in next input
			counter <= counter + 1;
		end
	end

	// STEP 2: Store input length for later use
	reg [8:0] length = counter; // 2^9 bits = 512, max we can store is 504 bits

	// STEP 3: Append 1 to the end of data and right pad with zeros
	reg [7:0] append = 8'b1000_0000;
	if (counter == 0) begin // No input case
		data_storage = {data_storage[511:504], append};
		data_storage[504:0] = 505'b0;
	end else begin
		data_storage = {data_storage[(counter*8)+7:8], append}; // append 1 with some padding
		counter <= counter + 1;
		// Shift it all to the left (right padding with 0)
		always @(posedge clk) begin
			if (input_complete && counter < 63) begin
				data_storage = {data_storage[(counter*8)+7:8], 8'b0}
				couner <= counter + 1;
			end
		end
	end

	// STEP 4: replace last 8 bits with length
	data_storage [7:0] = length;

	
	// HASHING THE DATA
	// STEP 5: Inital Hash constants
	// 		   SHA256 uses the first 32 bits of the fractional part of 
	//		   the square roots of the first 64 primes.
	//		   To start, I have initalized the hash values to the fractional
	//		   part of the first 8 primes.
	//		   We can then use the function SHA256_Constants to cycle through the rest

	reg [255:0] hash_values = {
		32'h6A09E667, 32'hBB67AE85, 32'h3C6EF372, 32'hA54FF53A,
		32'h510E527F, 32'h9B05688C, 32'h1F83D9AB, 32'h5BE0CD19
	};



	// STEP 6: Append 3 more 512 blocks of 0
	reg data_storage2 [2047:0] = {data_storage, 1536'b0}

	// STEP 7: Preform the first scramble
	SHA256_Scramble scramble_inst (
		.clock(clock),
		.data(data_storage2),
		.out(data_storage2)
	);


	// STEP 8: Compression Cycle
	// 		   Initalize the 8 hash values to our presets
	//         with our 64 word data, we will modify these hash values over and over






	// Initial block for simulation purposes
    initial begin
		data_storage = 512'b0;
		counter = 0;
		input_complete = 0;
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
	output reg [31:0] output_constant;
);

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


// This is the first right rotate module
// Our goal is to take a 32 bit (word) input from our 2048 bit dataset
// and for each word from [16-63] we will proform the following function
// 	word1 = (word1[i-15] rightrotate 7) xor (word1[i-15] rightrotate 18) xor (word1[i-15] righthift 3)
//	word2 = (word2[i-2] rightrotate 17) xor (word2[i-2] rightrotate 19) xor (word2[i-2] righthift 10)
// 	new_word = data_word[i-16] + word0 + data_word[i-7] + word1

module SHA256_Scramble (
	input wire clock,
	input wire [2047:0] data,
	output wire [2047:0] out
);
	always @(posedge clock) begin
		for (integer i = 16; i < 64; i = i + 1) begin
			reg [31:0] word0 = data[((i-15)*32)+31:((i-15)*32)]; // Get word0 from data
			assign word0 = ({word0[6:0], word0[31:7]} ^ {word0[17:0], word0[31:18]} ^ (word0 >> 3));

			reg [31:0] word1 = data[((i-2)*32)+31:((i-2)*32)]; // Get word1 from data
			assign word1 = ({word1[16:0], word1[31:17]} ^ {word1[18:0], word1[31:19]} ^ (word1 >> 10));

			assign data[(i*32)+31:i*32] = data[((i-16)*32)+31:((i-16)*32)] + word0 + data[((i-7)*32)+31:((i-7)*32)] + word1;
		end
	end
	assign out = data;
endmodule
