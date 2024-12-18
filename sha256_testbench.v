`timescale 1ns/1ps

module SHA256_testbench;
    // Testbench signals
    reg clock;
    reg reset;
    reg load_enable;
    reg input_complete;
    reg [7:0] input_data;

    wire [15:0] hashed_data;

    integer i = 0;

    SHA256 dut (
        .clock(clock),
        .reset(reset),
        .load_enable(load_enable),
        .input_complete(input_complete),
        .input_data(input_data),
        .hashed_data(hashed_data)
    );

    wire done = dut.output_handler.hash_ready;

    // Clock generation: 10ns peroid -> 100MHz
    initial begin
        clock = 0;
        forever #5 clock = ~clock;
    end
    
    // Apply reset and then load data
    initial begin
        // Initalize signals
        reset = 1;
        load_enable = 0;
        input_complete = 0;
        input_data = 8'h00;

        // Wait a couple of cycles
        #20;
        reset = 0;

        // Start loading the data
        load_enable = 1;

        // send binary data (one clock cycle per byte)
        input_data = 8'h47; // 'G'
        #10; 

        input_data = 8'h6F; // 'o'
        #10; 

        input_data = 8'h20; // ' '
        #10; 

        input_data = 8'h49; // 'I'
        #10; 

        input_data = 8'h72; // 'r'
        #10; 

        input_data = 8'h69; // 'i'
        #10; 

        input_data = 8'h73; // 's'
        #10; 

        input_data = 8'h68; // 'h'
        #10;

        input_data = 8'h21; // '!'
        #10;

        // So far we have only sent in 9 bytes
        //  so we will send in 23 bytes of 0's to get 256 bits total
        for (i = 0; i < 23; i = i + 1) begin 
            input_data = 8'h00;
            #10;
        end

        // Signal completion
        input_complete = 1;
        #10;

        // TEST CASE TWO -- numeric sequence 
        /*initial begin
            # 150 // will start after test case 1 
            // reset the system 
            reset = 1; 
            #20 reset = 0; 

            // send numbers 1 through 10
            integer i;
            for (i = 1; i <= 5; i = i + 1) begin 
                input_data = i[7:0]; // assign i to an 8 bit register 
                load_enable = 1; #10; load_enable = 0; #10;
            end
        end*/
    end

    // Monitor hashed_data changes to view our hashed data
    always @(posedge clock) begin
        if (dut.output_handler.hash_ready && !dut.output_handler.done) begin
            $display("Output Word: %h", hashed_data);
        end else if (dut.output_handler.done) begin
            $display("Final output segment: %h", hashed_data);
            $display("Hashing Complete. Check waveform or print statements for final hash.");
            $stop;
        end
    end
endmodule