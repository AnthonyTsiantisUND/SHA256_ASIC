/**********************************************
*	Module name: sha256_Stim
*	Pre-Conditions: none
*	Post-Conditions: ?
*	
*	This function generates binary values to
*	stimulate the sha256 encryption
*   Developed by Anthony Tsiantis, Natalie Sekerak and Olivia Zino
*   for CSE 30342 @ University of Notre Dame
*********************************************/ 


module SHA256_Stim;

    // Outputs to the SHA256 driver
    output logic clock, reset, load_enable, input_complete;
    output logic [7:0] input_data;

    parameter delay = 10; // Time delay between inputs

    // Counter to iterate through all 8-bit values
    reg [7:0] cnt;

    // Clock generation
    always #5 clock = ~clock;

    // Stimulus generation
    initial begin
        // Initialize signals
        clock = 0;
        reset = 1;
        load_enable = 0;
        input_complete = 0;
        cnt = 8'b0;

        // Reset SHA256 module
        #10 reset = 0;
        
        // Start loading data
        load_enable = 1;
        repeat (256) begin
            input_data = cnt; // Assign binary value from counter
            cnt = cnt + 1;
            #delay; // Wait for a delay to simulate clock cycle
        end
        
        // Signal input completion
        load_enable = 0;
        input_complete = 1;
        #delay input_complete = 0;

        // Finish simulation
        #200;
        $stop;
    end

    // Monitor the inputs
    initial begin
        $monitor("Time: %0d | Clock: %b | Reset: %b | Load Enable: %b | Input Data: %b | Input Complete: %b",
                 $time, clock, reset, load_enable, input_data, input_complete);
    end

endmodule
