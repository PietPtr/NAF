`timescale 1ns / 1ps

module boilerplate (
    input wire clk,
    input wire rst, 

    input wire valid_rx,
    output wire ready_rx,
    input wire first_rx,
    input wire last_rx,
    input wire [7:0] payload_rx,
    input wire last_be_rx,

    output wire valid_tx,
    input wire ready_tx,
    output wire first_tx,
    output wire last_tx,
    output wire [7:0] payload_tx,
    output wire last_be_tx
);

    always @(posedge clk) begin
        if (valid_rx) begin
            $display("%0t | RX Data: Valid: %b, Ready: %b, First: %b, Last: %b, Payload: %h, Last BE: %b", 
                    $time, valid_rx, ready_rx, first_rx, last_rx, payload_rx, last_be_rx);
        end
        if (valid_tx) begin
            $display("%0t | TX Data: Valid: %b, Ready: %b, First: %b, Last: %b, Payload: %h, Last BE: %b", 
                    $time, valid_tx, ready_tx, first_tx, last_tx, payload_tx, last_be_tx);
        end
    end

    main main(
        .clock(clk),
        .reset(rst),
        .enable(1),
        .udp_rx_valid(valid_rx),
        .udp_rx_first(first_rx),
        .udp_rx_last(last_rx),
        .udp_rx_payload(payload_rx),
        .udp_rx_last_be(last_be_rx),
        .udp_tx_ready(ready_tx),

        .udp_tx_valid(valid_tx),
        .udp_tx_first(first_tx),
        .udp_tx_last(last_tx),
        .udp_tx_payload(payload_tx),
        .udp_rx_ready(ready_rx)
    );

    assign last_be_tx = 1'b1;
endmodule
