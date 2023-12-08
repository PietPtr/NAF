module boilerplate (
    input wire valid_rx,
    output wire ready_rx,
    input wire first_rx,
    input wire last_rx,
    input wire [31:0] payload_rx,

    output wire valid_tx,
    input wire ready_tx,
    output wire first_tx,
    output wire last_tx,
    output wire [31:0] payload_tx,
);
    // Module implementation goes here
    // assign valid_tx = valid_rx;
    // assign ready_rx = ready_tx;
    // assign first_tx = first_rx;
    // assign last_tx = last_rx;
    // assign payload_tx = payload_rx;
endmodule
