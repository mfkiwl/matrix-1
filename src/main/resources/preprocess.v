module preprocess (
   input    [63 : 0] dividend ,
   input    [63 : 0] divisor ,
   output   [5  : 0] iter,
   output   [66 : 0] format_divisor,
   output   [69 : 0] format_dividend,
   output   [5  : 0] recovery
);

reg [63 : 0] divisor_tmp;
reg [66 : 0] dividend_tmp;
reg [5  : 0] recovery_tmp;
reg [5  : 0] iter_tmp;

always @(*) begin
    casex (divisor)
      64'b1xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx:
      begin
      	divisor_tmp = divisor;
      	dividend_tmp = {2'b0,dividend,1'b0};
      	recovery_tmp = 64;
      	iter_tmp = 1;
      end
      64'b01xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx:
      begin
      	divisor_tmp = {divisor[62:0], 1'b0};
      	dividend_tmp = {3'b0, dividend};
      	recovery_tmp = 63;
      	iter_tmp = 2;
      end
      64'b001xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx:
      begin
      	divisor_tmp = {divisor[61:0], 2'b0};
      	dividend_tmp = {2'b0,dividend,1'b0};
      	recovery_tmp = 62;
      	iter_tmp = 2;
      end
      64'b0001xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx:
      begin
      	divisor_tmp = {divisor[60:0], 3'b0};
      	dividend_tmp = {3'b0, dividend};
      	recovery_tmp = 61;
      	iter_tmp = 3;
      end
      64'b00001xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx:
      begin
      	divisor_tmp = {divisor[59:0], 4'b0};
      	dividend_tmp = {2'b0,dividend,1'b0};
      	recovery_tmp = 60;
      	iter_tmp = 3;
      end
      64'b000001xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx:
      begin
      	divisor_tmp = {divisor[58:0], 5'b0};
      	dividend_tmp = {3'b0, dividend};
      	recovery_tmp = 59;
      	iter_tmp = 4;
      end
      64'b0000001xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx:
      begin
      	divisor_tmp = {divisor[57:0], 6'b0};
      	dividend_tmp = {2'b0,dividend,1'b0};
      	recovery_tmp = 58;
      	iter_tmp = 4;
      end
      64'b00000001xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx:
      begin
      	divisor_tmp = {divisor[56:0], 7'b0};
      	dividend_tmp = {3'b0, dividend};
      	recovery_tmp = 57;
      	iter_tmp = 5;
      end
      64'b000000001xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx:
      begin
      	divisor_tmp = {divisor[55:0], 8'b0};
      	dividend_tmp = {2'b0,dividend,1'b0};
      	recovery_tmp = 56;
      	iter_tmp = 5;
      end
      64'b0000000001xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx:
      begin
      	divisor_tmp = {divisor[54:0], 9'b0};
      	dividend_tmp = {3'b0, dividend};
      	recovery_tmp = 55;
      	iter_tmp = 6;
      end
      64'b00000000001xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx:
      begin
      	divisor_tmp = {divisor[53:0], 10'b0};
      	dividend_tmp = {2'b0,dividend,1'b0};
      	recovery_tmp = 54;
      	iter_tmp = 6;
      end
      64'b000000000001xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx:
      begin
      	divisor_tmp = {divisor[52:0], 11'b0};
      	dividend_tmp = {3'b0, dividend};
      	recovery_tmp = 53;
      	iter_tmp = 7;
      end
      64'b0000000000001xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx:
      begin
      	divisor_tmp = {divisor[51:0], 12'b0};
      	dividend_tmp = {2'b0,dividend,1'b0};
      	recovery_tmp = 52;
      	iter_tmp = 7;
      end
      64'b00000000000001xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx:
      begin
      	divisor_tmp = {divisor[50:0], 13'b0};
      	dividend_tmp = {3'b0, dividend};
      	recovery_tmp = 51;
      	iter_tmp = 8;
      end
      64'b000000000000001xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx:
      begin
      	divisor_tmp = {divisor[49:0], 14'b0};
      	dividend_tmp = {2'b0,dividend,1'b0};
      	recovery_tmp = 50;
      	iter_tmp = 8;
      end
      64'b0000000000000001xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx:
      begin
      	divisor_tmp = {divisor[48:0], 15'b0};
      	dividend_tmp = {3'b0, dividend};
      	recovery_tmp = 49;
      	iter_tmp = 9;
      end
      64'b00000000000000001xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx:
      begin
      	divisor_tmp = {divisor[47:0], 16'b0};
      	dividend_tmp = {2'b0,dividend,1'b0};
      	recovery_tmp = 48;
      	iter_tmp = 9;
      end
      64'b000000000000000001xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx:
      begin
      	divisor_tmp = {divisor[46:0], 17'b0};
      	dividend_tmp = {3'b0, dividend};
      	recovery_tmp = 47;
      	iter_tmp = 10;
      end
      64'b0000000000000000001xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx:
      begin
      	divisor_tmp = {divisor[45:0], 18'b0};
      	dividend_tmp = {2'b0,dividend,1'b0};
      	recovery_tmp = 46;
      	iter_tmp = 10;
      end
      64'b00000000000000000001xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx:
      begin
      	divisor_tmp = {divisor[44:0], 19'b0};
      	dividend_tmp = {3'b0, dividend};
      	recovery_tmp = 45;
      	iter_tmp = 11;
      end
      64'b000000000000000000001xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx:
      begin
      	divisor_tmp = {divisor[43:0], 20'b0};
      	dividend_tmp = {2'b0,dividend,1'b0};
      	recovery_tmp = 44;
      	iter_tmp = 11;
      end
      64'b0000000000000000000001xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx:
      begin
      	divisor_tmp = {divisor[42:0], 21'b0};
      	dividend_tmp = {3'b0, dividend};
      	recovery_tmp = 43;
      	iter_tmp = 12;
      end
      64'b00000000000000000000001xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx:
      begin
      	divisor_tmp = {divisor[41:0], 22'b0};
      	dividend_tmp = {2'b0,dividend,1'b0};
      	recovery_tmp = 42;
      	iter_tmp = 12;
      end
      64'b000000000000000000000001xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx:
      begin
      	divisor_tmp = {divisor[40:0], 23'b0};
      	dividend_tmp = {3'b0, dividend};
      	recovery_tmp = 41;
      	iter_tmp = 13;
      end
      64'b0000000000000000000000001xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx:
      begin
      	divisor_tmp = {divisor[39:0], 24'b0};
      	dividend_tmp = {2'b0,dividend,1'b0};
      	recovery_tmp = 40;
      	iter_tmp = 13;
      end
      64'b00000000000000000000000001xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx:
      begin
      	divisor_tmp = {divisor[38:0], 25'b0};
      	dividend_tmp = {3'b0, dividend};
      	recovery_tmp = 39;
      	iter_tmp = 14;
      end
      64'b000000000000000000000000001xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx:
      begin
      	divisor_tmp = {divisor[37:0], 26'b0};
      	dividend_tmp = {2'b0,dividend,1'b0};
      	recovery_tmp = 38;
      	iter_tmp = 14;
      end
      64'b0000000000000000000000000001xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx:
      begin
      	divisor_tmp = {divisor[36:0], 27'b0};
      	dividend_tmp = {3'b0, dividend};
      	recovery_tmp = 37;
      	iter_tmp = 15;
      end
      64'b00000000000000000000000000001xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx:
      begin
      	divisor_tmp = {divisor[35:0], 28'b0};
      	dividend_tmp = {2'b0,dividend,1'b0};
      	recovery_tmp = 36;
      	iter_tmp = 15;
      end
      64'b000000000000000000000000000001xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx:
      begin
      	divisor_tmp = {divisor[34:0], 29'b0};
      	dividend_tmp = {3'b0, dividend};
      	recovery_tmp = 35;
      	iter_tmp = 16;
      end
      64'b0000000000000000000000000000001xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx:
      begin
      	divisor_tmp = {divisor[33:0], 30'b0};
      	dividend_tmp = {2'b0,dividend,1'b0};
      	recovery_tmp = 34;
      	iter_tmp = 16;
      end
      64'b00000000000000000000000000000001xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx:
      begin
      	divisor_tmp = {divisor[32:0], 31'b0};
      	dividend_tmp = {3'b0, dividend};
      	recovery_tmp = 33;
      	iter_tmp = 17;
      end
      64'b000000000000000000000000000000001xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx:
      begin
      	divisor_tmp = {divisor[31:0], 32'b0};
      	dividend_tmp = {2'b0,dividend,1'b0};
      	recovery_tmp = 32;
      	iter_tmp = 17;
      end
      64'b0000000000000000000000000000000001xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx:
      begin
      	divisor_tmp = {divisor[30:0], 33'b0};
      	dividend_tmp = {3'b0, dividend};
      	recovery_tmp = 31;
      	iter_tmp = 18;
      end
      64'b00000000000000000000000000000000001xxxxxxxxxxxxxxxxxxxxxxxxxxxxx:
      begin
      	divisor_tmp = {divisor[29:0], 34'b0};
      	dividend_tmp = {2'b0,dividend,1'b0};
      	recovery_tmp = 30;
      	iter_tmp = 18;
      end
      64'b000000000000000000000000000000000001xxxxxxxxxxxxxxxxxxxxxxxxxxxx:
      begin
      	divisor_tmp = {divisor[28:0], 35'b0};
      	dividend_tmp = {3'b0, dividend};
      	recovery_tmp = 29;
      	iter_tmp = 19;
      end
      64'b0000000000000000000000000000000000001xxxxxxxxxxxxxxxxxxxxxxxxxxx:
      begin
      	divisor_tmp = {divisor[27:0], 36'b0};
      	dividend_tmp = {2'b0,dividend,1'b0};
      	recovery_tmp = 28;
      	iter_tmp = 19;
      end
      64'b00000000000000000000000000000000000001xxxxxxxxxxxxxxxxxxxxxxxxxx:
      begin
      	divisor_tmp = {divisor[26:0], 37'b0};
      	dividend_tmp = {3'b0, dividend};
      	recovery_tmp = 27;
      	iter_tmp = 20;
      end
      64'b000000000000000000000000000000000000001xxxxxxxxxxxxxxxxxxxxxxxxx:
      begin
      	divisor_tmp = {divisor[25:0], 38'b0};
      	dividend_tmp = {2'b0,dividend,1'b0};
      	recovery_tmp = 26;
      	iter_tmp = 20;
      end
      64'b0000000000000000000000000000000000000001xxxxxxxxxxxxxxxxxxxxxxxx:
      begin
      	divisor_tmp = {divisor[24:0], 39'b0};
      	dividend_tmp = {3'b0, dividend};
      	recovery_tmp = 25;
      	iter_tmp = 21;
      end
      64'b00000000000000000000000000000000000000001xxxxxxxxxxxxxxxxxxxxxxx:
      begin
      	divisor_tmp = {divisor[23:0], 40'b0};
      	dividend_tmp = {2'b0,dividend,1'b0};
      	recovery_tmp = 24;
      	iter_tmp = 21;
      end
      64'b000000000000000000000000000000000000000001xxxxxxxxxxxxxxxxxxxxxx:
      begin
      	divisor_tmp = {divisor[22:0], 41'b0};
      	dividend_tmp = {3'b0, dividend};
      	recovery_tmp = 23;
      	iter_tmp = 22;
      end
      64'b0000000000000000000000000000000000000000001xxxxxxxxxxxxxxxxxxxxx:
      begin
      	divisor_tmp = {divisor[21:0], 42'b0};
      	dividend_tmp = {2'b0,dividend,1'b0};
      	recovery_tmp = 22;
      	iter_tmp = 22;
      end
      64'b00000000000000000000000000000000000000000001xxxxxxxxxxxxxxxxxxxx:
      begin
      	divisor_tmp = {divisor[20:0], 43'b0};
      	dividend_tmp = {3'b0, dividend};
      	recovery_tmp = 21;
      	iter_tmp = 23;
      end
      64'b000000000000000000000000000000000000000000001xxxxxxxxxxxxxxxxxxx:
      begin
      	divisor_tmp = {divisor[19:0], 44'b0};
      	dividend_tmp = {2'b0,dividend,1'b0};
      	recovery_tmp = 20;
      	iter_tmp = 23;
      end
      64'b0000000000000000000000000000000000000000000001xxxxxxxxxxxxxxxxxx:
      begin
      	divisor_tmp = {divisor[18:0], 45'b0};
      	dividend_tmp = {3'b0, dividend};
      	recovery_tmp = 19;
      	iter_tmp = 24;
      end
      64'b00000000000000000000000000000000000000000000001xxxxxxxxxxxxxxxxx:
      begin
      	divisor_tmp = {divisor[17:0], 46'b0};
      	dividend_tmp = {2'b0,dividend,1'b0};
      	recovery_tmp = 18;
      	iter_tmp = 24;
      end
      64'b000000000000000000000000000000000000000000000001xxxxxxxxxxxxxxxx:
      begin
      	divisor_tmp = {divisor[16:0], 47'b0};
      	dividend_tmp = {3'b0, dividend};
      	recovery_tmp = 17;
      	iter_tmp = 25;
      end
      64'b0000000000000000000000000000000000000000000000001xxxxxxxxxxxxxxx:
      begin
      	divisor_tmp = {divisor[15:0], 48'b0};
      	dividend_tmp = {2'b0,dividend,1'b0};
      	recovery_tmp = 16;
      	iter_tmp = 25;
      end
      64'b00000000000000000000000000000000000000000000000001xxxxxxxxxxxxxx:
      begin
      	divisor_tmp = {divisor[14:0], 49'b0};
      	dividend_tmp = {3'b0, dividend};
      	recovery_tmp = 15;
      	iter_tmp = 26;
      end
      64'b000000000000000000000000000000000000000000000000001xxxxxxxxxxxxx:
      begin
      	divisor_tmp = {divisor[13:0], 50'b0};
      	dividend_tmp = {2'b0,dividend,1'b0};
      	recovery_tmp = 14;
      	iter_tmp = 26;
      end
      64'b0000000000000000000000000000000000000000000000000001xxxxxxxxxxxx:
      begin
      	divisor_tmp = {divisor[12:0], 51'b0};
      	dividend_tmp = {3'b0, dividend};
      	recovery_tmp = 13;
      	iter_tmp = 27;
      end
      64'b00000000000000000000000000000000000000000000000000001xxxxxxxxxxx:
      begin
      	divisor_tmp = {divisor[11:0], 52'b0};
      	dividend_tmp = {2'b0,dividend,1'b0};
      	recovery_tmp = 12;
      	iter_tmp = 27;
      end
      64'b000000000000000000000000000000000000000000000000000001xxxxxxxxxx:
      begin
      	divisor_tmp = {divisor[10:0], 53'b0};
      	dividend_tmp = {3'b0, dividend};
      	recovery_tmp = 11;
      	iter_tmp = 28;
      end
      64'b0000000000000000000000000000000000000000000000000000001xxxxxxxxx:
      begin
      	divisor_tmp = {divisor[9:0], 54'b0};
      	dividend_tmp = {2'b0,dividend,1'b0};
      	recovery_tmp = 10;
      	iter_tmp = 28;
      end
      64'b00000000000000000000000000000000000000000000000000000001xxxxxxxx:
      begin
      	divisor_tmp = {divisor[8:0], 55'b0};
      	dividend_tmp = {3'b0, dividend};
      	recovery_tmp = 9;
      	iter_tmp = 29;
      end
      64'b000000000000000000000000000000000000000000000000000000001xxxxxxx:
      begin
      	divisor_tmp = {divisor[7:0], 56'b0};
      	dividend_tmp = {2'b0,dividend,1'b0};
      	recovery_tmp = 8;
      	iter_tmp = 29;
      end
      64'b0000000000000000000000000000000000000000000000000000000001xxxxxx:
      begin
      	divisor_tmp = {divisor[6:0], 57'b0};
      	dividend_tmp = {3'b0, dividend};
      	recovery_tmp = 7;
      	iter_tmp = 30;
      end
      64'b00000000000000000000000000000000000000000000000000000000001xxxxx:
      begin
      	divisor_tmp = {divisor[5:0], 58'b0};
      	dividend_tmp = {2'b0,dividend,1'b0};
      	recovery_tmp = 6;
      	iter_tmp = 30;
      end
      64'b000000000000000000000000000000000000000000000000000000000001xxxx:
      begin
      	divisor_tmp = {divisor[4:0], 59'b0};
      	dividend_tmp = {3'b0, dividend};
      	recovery_tmp = 5;
      	iter_tmp = 31;
      end
      64'b0000000000000000000000000000000000000000000000000000000000001xxx:
      begin
      	divisor_tmp = {divisor[3:0], 60'b0};
      	dividend_tmp = {2'b0,dividend,1'b0};
      	recovery_tmp = 4;
      	iter_tmp = 31;
      end
      64'b00000000000000000000000000000000000000000000000000000000000001xx:
      begin
      	divisor_tmp = {divisor[2:0], 61'b0};
      	dividend_tmp = {3'b0, dividend};
      	recovery_tmp = 3;
      	iter_tmp = 32;
      end
      64'b000000000000000000000000000000000000000000000000000000000000001x:
      begin
      	divisor_tmp = {divisor[1:0], 62'b0};
      	dividend_tmp = {2'b0,dividend,1'b0};
      	recovery_tmp = 2;
      	iter_tmp = 32;
      end
      64'b0000000000000000000000000000000000000000000000000000000000000001:
      begin
      	divisor_tmp = {divisor[0],63'b0};
      	dividend_tmp = {3'b0, dividend};
      	recovery_tmp = 1;
      	iter_tmp = 33;
      end
      64'b0000000000000000000000000000000000000000000000000000000000000000:
       begin
           divisor_tmp = 0;
           dividend_tmp = 0;
           recovery_tmp = 0;
           iter_tmp = 0;
       end
       default:
       begin
          divisor_tmp = 0;
          dividend_tmp = 0;
          recovery_tmp = 0;
          iter_tmp = 0;
       end
    endcase
end

assign format_dividend = {3'b0, dividend_tmp};
assign format_divisor = {3'b0, divisor_tmp}
assign iter = iter_tmp;
assign recovery = recovery_tmp;

endmodule // preprocess