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

        // Signal completion
        input_complete = 1;
        #10;

        /*// TEST CASE TWO -- numeric sequence 
        initial begin
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
    reg temp = 1;
    wire ready = dut.output_handler.read_enable;
    wire done = dut.output_handler.done;
    always @(posedge clock) begin
        if (temp) begin
            $display("");
            $display("Input String: Go Irish!");
            $write("Output Hash: ");
            temp <= 0;
        end if (ready && !done) begin
            $write(" %h", hashed_data);
        end else if (done) begin
            $write(" %h", hashed_data);
            $display("");
            $display("Correct Hash: 60c9 b396 2375 4d14 4454 fd59 fac8 42d3 80ab 71b8 f824 c3b1 2afd fe46 be40 4e4e ");

            $display("");
            $stop;
        end
    end
endmodule