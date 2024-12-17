/********************************************************************************************
*	Module name: SHA256_Driver
*	Pre-Conditions: none
*	Post-Conditions: none
*	
*	This is the main driver function for generating
*	stimulus and simulating the SHA256 encryption algorithm.
*   Developed by Anthony Tsiantis, Natalie Sekerak and Olivia Zino
*   for CSE 30342 @ University of Notre Dame
*******************************************************************************************/ 
module SHA256_Driver; 

    // test bench signals 
    reg clock;
    reg reset; 
    reg load_enable;
    reg input_complete; 
    reg [7:0] input_data; 
    wire [15:0] hashed_data; 

    // instantiate the SHA256 Module 
    SHA256 sha256_instance ( 
        .clock(clock),
        .reset(reset),
        .load_enable(load_enable),
        .input_complete(input_complete),
        .input_data(input_data),
        .hashed_data(hashed_data)
    );

    // clock 
    always # 5 clock = ~clock; 

    // TEST CASE ONE -- hardcoded input 
    initial begin 
        // initialize signals 
        clock = 0; 
        reset = 0;
        load_enable = 0; 
        input_complete = 0; 
        input_data = 8'b0; 

        // release reset
        #10 reset = 0; 

        // load input data (using string "goirish") 
            // ASCII translation: 103 111 105 114 105 115 104
            // BINARY: 01100111 01101111 01101001 01110010 01101001 01110011 01101000 00001010
        #10 load_enable = 1; 

        // send binary data 
        input_data = 8'b01100111; // 'g'
        #10; 

        input_data = 8'b01101111; // 'o'
        #10; 

        input_data = 8'b01101001; // 'i'
        #10; 

        input_data = 8'b01110010; // 'r'
        #10; 

        input_data = 8'b01101001; // 'i'
        #10; 

        input_data = 8'b01110011; // 's'
        #10; 

        input_data = 8'b01101000; // 'h'
        #10;

        input_data = 8'b00001010;  // '\n' (newline character for end of line)
        #10;

        // indicate that the input transmission has completed 
        load_enable = 0; 
        input_complete = 1; 
        #10 input_complete = 0; 

        // wait for output
        # 200; 

        // finish 
        $stop
    end

    // TEST CASE TWO -- numeric sequence 
    initial begin
        # 150 // will start after test case 1 
        // reset the system 
        reset = 1; 
        #20 reset = 0; 

        // send numbers 1 through 10
        for (i = 1; i <= 5; i = i + 1) begin 
            input_data = i[7:0]; // assign i to an 8 bit register 
            load_enable = 1; #10; load_enab;e = 0; #10;
        end
    end

    // TEST CASE THREE -- use the stim.v file
    initial begin 
        #300; // will start after test case 2  
        integer file, code; 
endmodule

// watch the output --> will be printed out
    initial begin 
        $monitor("time: %0d | reset: %b | load enable: %b | input data: %b | hashed data: %h",
                 $time, reset, load_enable, input_data, hashed_data);
    end