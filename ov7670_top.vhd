------------------------------------------------------------------
--Copyright 2017 Andrey S. Ionisyan (anserion@gmail.com)
--Licensed under the Apache License, Version 2.0 (the "License");
--you may not use this file except in compliance with the License.
--You may obtain a copy of the License at
--    http://www.apache.org/licenses/LICENSE-2.0
--Unless required by applicable law or agreed to in writing, software
--distributed under the License is distributed on an "AS IS" BASIS,
--WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--See the License for the specific language governing permissions and
--limitations under the License.
------------------------------------------------------------------

----------------------------------------------------------------------------------
-- Engineer: Andrey S. Ionisyan <anserion@gmail.com>
-- 
-- Description: Top level for the OV7670 camera project (Alinx AX516 board).
----------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

entity ov7670_top is
   generic(
    C3_P0_MASK_SIZE           : integer := 8;
    C3_P0_DATA_PORT_SIZE      : integer := 64;
    C3_P1_MASK_SIZE           : integer := 8;
    C3_P1_DATA_PORT_SIZE      : integer := 64;
    C3_MEMCLK_PERIOD          : integer := 3200;
    C3_RST_ACT_LOW            : integer := 1;
    C3_INPUT_CLK_TYPE         : string := "SINGLE_ENDED";
    C3_CALIB_SOFT_IP          : string := "TRUE";
    C3_SIMULATION             : string := "FALSE";
    DEBUG_EN                  : integer := 0;
    C3_MEM_ADDR_ORDER         : string := "ROW_BANK_COLUMN";
    C3_NUM_DQ_PINS            : integer := 16;
    C3_MEM_ADDR_WIDTH         : integer := 13;
    C3_MEM_BANKADDR_WIDTH     : integer := 3
   );
    Port ( 
		clk50_ucf    : in    STD_LOGIC;

      mcb3_dram_dq      : inout  std_logic_vector(15 downto 0);
      mcb3_dram_a       : out std_logic_vector(12 downto 0);
      mcb3_dram_ba      : out std_logic_vector(2 downto 0);
      mcb3_dram_ras_n   : out std_logic;
      mcb3_dram_cas_n   : out std_logic;
      mcb3_dram_we_n    : out std_logic;
      mcb3_dram_odt     : out std_logic;
      mcb3_dram_reset_n : out std_logic;
      mcb3_dram_cke     : out std_logic;
      mcb3_dram_dm      : out std_logic;
      mcb3_dram_udqs    : inout  std_logic;
      mcb3_dram_udqs_n  : inout  std_logic;
      mcb3_rzq          : inout  std_logic;
      mcb3_zio          : inout  std_logic;
      mcb3_dram_udm     : out std_logic;
      mcb3_dram_dqs     : inout  std_logic;
      mcb3_dram_dqs_n   : inout  std_logic;
      mcb3_dram_ck      : out std_logic;
      mcb3_dram_ck_n    : out std_logic;
   
		OV7670_SIOC  : out   STD_LOGIC;
		OV7670_SIOD  : inout STD_LOGIC;
		OV7670_RESET : out   STD_LOGIC;
		OV7670_PWDN  : out   STD_LOGIC;
		OV7670_VSYNC : in    STD_LOGIC;
		OV7670_HREF  : in    STD_LOGIC;
		OV7670_PCLK  : in    STD_LOGIC;
		OV7670_XCLK  : out   STD_LOGIC;
		OV7670_D     : in    STD_LOGIC_VECTOR(7 downto 0);

      lcd_red      : out   STD_LOGIC_VECTOR(7 downto 0);
      lcd_green    : out   STD_LOGIC_VECTOR(7 downto 0);
      lcd_blue     : out   STD_LOGIC_VECTOR(7 downto 0);
      lcd_hsync    : out   STD_LOGIC;
      lcd_vsync    : out   STD_LOGIC;
      lcd_dclk     : out   STD_LOGIC;

		vga_red      : out STD_LOGIC_VECTOR(4 downto 0);
		vga_green    : out STD_LOGIC_VECTOR(5 downto 0);
		vga_blue     : out STD_LOGIC_VECTOR(4 downto 0);
		vga_hsync    : out STD_LOGIC;
		vga_vsync    : out STD_LOGIC;
		
		LED          : out   STD_LOGIC_VECTOR(3 downto 0);
		btn			 : in 	STD_LOGIC_VECTOR(3 downto 0);
		reset_n	    : in    STD_LOGIC
	 );
end ov7670_top;

architecture Behavioral of ov7670_top is
   component clocking_core
   port (
      CLK_in : in  std_logic;
      CLK_100    : out std_logic;
      CLK_50   : out std_logic;
      CLK_25    : out std_logic;
      CLK_10    : out std_logic
      );
   end component;

COMPONENT DDR_LCD_scanline
  PORT (
    clka : IN STD_LOGIC;
    wea : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
    addra : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
    dina : IN STD_LOGIC_VECTOR(63 DOWNTO 0);
    clkb : IN STD_LOGIC;
    addrb : IN STD_LOGIC_VECTOR(9 DOWNTO 0);
    doutb : OUT STD_LOGIC_VECTOR(15 DOWNTO 0)
  );
END COMPONENT;

COMPONENT CAM_DDR_scanline
  PORT (
    clka : IN STD_LOGIC;
    wea : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
    addra : IN STD_LOGIC_VECTOR(9 DOWNTO 0);
    dina : IN STD_LOGIC_VECTOR(15 DOWNTO 0);
    clkb : IN STD_LOGIC;
    addrb : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
    doutb : OUT STD_LOGIC_VECTOR(63 DOWNTO 0)
  );
END COMPONENT;

   component ov7670_capture is
    Port (
      en    : in std_logic;
      clk   : in std_logic;
      vsync : in std_logic;
		href  : in std_logic;
		din   : in std_logic_vector(7 downto 0);
      cam_x : out std_logic_vector(9 downto 0);
      cam_y : out std_logic_vector(9 downto 0);
      pixel : out std_logic_vector(15 downto 0);
      ready : out std_logic
		);
   end component;

   COMPONENT lcd_AN430
    Port ( 
      en      : in std_logic;
		clk     : in  STD_LOGIC;
		red     : out STD_LOGIC_VECTOR(7 downto 0);
		green   : out STD_LOGIC_VECTOR(7 downto 0);
		blue    : out STD_LOGIC_VECTOR(7 downto 0);
		hsync   : out STD_LOGIC;
		vsync   : out STD_LOGIC;
		de	     : out STD_LOGIC;
      x       : out STD_LOGIC_VECTOR(9 downto 0);
      y       : out STD_LOGIC_VECTOR(9 downto 0);
      dirty_x : out STD_LOGIC_VECTOR(9 downto 0);
      dirty_y : out STD_LOGIC_VECTOR(9 downto 0);
      pixel   : in STD_LOGIC_VECTOR(23 downto 0);
		char_x    : out STD_LOGIC_VECTOR(6 downto 0);
		char_y	 : out STD_LOGIC_VECTOR(4 downto 0);
		char_code : in  STD_LOGIC_VECTOR(7 downto 0)
	 );
	END COMPONENT;

   COMPONENT vga
    Port (
      en    : in std_logic;
		clk   : in  STD_LOGIC;
		red   : out STD_LOGIC_VECTOR(4 downto 0);
		green : out STD_LOGIC_VECTOR(5 downto 0);
		blue  : out STD_LOGIC_VECTOR(4 downto 0);
		hsync : out STD_LOGIC;
		vsync : out STD_LOGIC;
      de    : out STD_LOGIC;
		x     : out STD_LOGIC_VECTOR(9 downto 0);
		y     : out STD_LOGIC_VECTOR(9 downto 0);
      dirty_x : out STD_LOGIC_VECTOR(9 downto 0);
      dirty_y : out STD_LOGIC_VECTOR(9 downto 0);
      pixel :  in STD_LOGIC_VECTOR(15 downto 0);
		char_x    : out STD_LOGIC_VECTOR(6 downto 0);
		char_y	 : out STD_LOGIC_VECTOR(4 downto 0);
		char_code : in  STD_LOGIC_VECTOR(7 downto 0)
	 );
   END COMPONENT;

component mig_ddr3
 generic(
    C3_P0_MASK_SIZE           : integer := 8;
    C3_P0_DATA_PORT_SIZE      : integer := 64;
    C3_P1_MASK_SIZE           : integer := 8;
    C3_P1_DATA_PORT_SIZE      : integer := 64;
    C3_MEMCLK_PERIOD          : integer := 3200;
    C3_RST_ACT_LOW            : integer := 1;
    C3_INPUT_CLK_TYPE         : string := "SINGLE_ENDED";
    C3_CALIB_SOFT_IP          : string := "TRUE";
    C3_SIMULATION             : string := "FALSE";
    DEBUG_EN                  : integer := 0;
    C3_MEM_ADDR_ORDER         : string := "ROW_BANK_COLUMN";
    C3_NUM_DQ_PINS            : integer := 16;
    C3_MEM_ADDR_WIDTH         : integer := 13;
    C3_MEM_BANKADDR_WIDTH     : integer := 3
);
    port (
   mcb3_dram_dq                            : inout  std_logic_vector(C3_NUM_DQ_PINS-1 downto 0);
   mcb3_dram_a                             : out std_logic_vector(C3_MEM_ADDR_WIDTH-1 downto 0);
   mcb3_dram_ba                            : out std_logic_vector(C3_MEM_BANKADDR_WIDTH-1 downto 0);
   mcb3_dram_ras_n                         : out std_logic;
   mcb3_dram_cas_n                         : out std_logic;
   mcb3_dram_we_n                          : out std_logic;
   mcb3_dram_odt                           : out std_logic;
   mcb3_dram_reset_n                       : out std_logic;
   mcb3_dram_cke                           : out std_logic;
   mcb3_dram_dm                            : out std_logic;
   mcb3_dram_udqs                          : inout  std_logic;
   mcb3_dram_udqs_n                        : inout  std_logic;
   mcb3_rzq                                : inout  std_logic;
   mcb3_zio                                : inout  std_logic;
   mcb3_dram_udm                           : out std_logic;
   c3_sys_clk                              : in  std_logic;
   c3_sys_rst_i                            : in  std_logic;
   c3_calib_done                           : out std_logic;
   c3_clk0                                 : out std_logic;
   c3_rst0                                 : out std_logic;
   mcb3_dram_dqs                           : inout  std_logic;
   mcb3_dram_dqs_n                         : inout  std_logic;
   mcb3_dram_ck                            : out std_logic;
   mcb3_dram_ck_n                          : out std_logic;
   c3_p0_cmd_clk                           : in std_logic;
   c3_p0_cmd_en                            : in std_logic;
   c3_p0_cmd_instr                         : in std_logic_vector(2 downto 0);
   c3_p0_cmd_bl                            : in std_logic_vector(5 downto 0);
   c3_p0_cmd_byte_addr                     : in std_logic_vector(29 downto 0);
   c3_p0_cmd_empty                         : out std_logic;
   c3_p0_cmd_full                          : out std_logic;
   c3_p0_wr_clk                            : in std_logic;
   c3_p0_wr_en                             : in std_logic;
   c3_p0_wr_mask                           : in std_logic_vector(C3_P0_MASK_SIZE - 1 downto 0);
   c3_p0_wr_data                           : in std_logic_vector(C3_P0_DATA_PORT_SIZE - 1 downto 0);
   c3_p0_wr_full                           : out std_logic;
   c3_p0_wr_empty                          : out std_logic;
   c3_p0_wr_count                          : out std_logic_vector(6 downto 0);
   c3_p0_wr_underrun                       : out std_logic;
   c3_p0_wr_error                          : out std_logic;
   c3_p0_rd_clk                            : in std_logic;
   c3_p0_rd_en                             : in std_logic;
   c3_p0_rd_data                           : out std_logic_vector(C3_P0_DATA_PORT_SIZE - 1 downto 0);
   c3_p0_rd_full                           : out std_logic;
   c3_p0_rd_empty                          : out std_logic;
   c3_p0_rd_count                          : out std_logic_vector(6 downto 0);
   c3_p0_rd_overflow                       : out std_logic;
   c3_p0_rd_error                          : out std_logic;
   c3_p1_cmd_clk                           : in std_logic;
   c3_p1_cmd_en                            : in std_logic;
   c3_p1_cmd_instr                         : in std_logic_vector(2 downto 0);
   c3_p1_cmd_bl                            : in std_logic_vector(5 downto 0);
   c3_p1_cmd_byte_addr                     : in std_logic_vector(29 downto 0);
   c3_p1_cmd_empty                         : out std_logic;
   c3_p1_cmd_full                          : out std_logic;
   c3_p1_wr_clk                            : in std_logic;
   c3_p1_wr_en                             : in std_logic;
   c3_p1_wr_mask                           : in std_logic_vector(C3_P1_MASK_SIZE - 1 downto 0);
   c3_p1_wr_data                           : in std_logic_vector(C3_P1_DATA_PORT_SIZE - 1 downto 0);
   c3_p1_wr_full                           : out std_logic;
   c3_p1_wr_empty                          : out std_logic;
   c3_p1_wr_count                          : out std_logic_vector(6 downto 0);
   c3_p1_wr_underrun                       : out std_logic;
   c3_p1_wr_error                          : out std_logic;
   c3_p1_rd_clk                            : in std_logic;
   c3_p1_rd_en                             : in std_logic;
   c3_p1_rd_data                           : out std_logic_vector(C3_P1_DATA_PORT_SIZE - 1 downto 0);
   c3_p1_rd_full                           : out std_logic;
   c3_p1_rd_empty                          : out std_logic;
   c3_p1_rd_count                          : out std_logic_vector(6 downto 0);
   c3_p1_rd_overflow                       : out std_logic;
   c3_p1_rd_error                          : out std_logic
);
end component;

signal c3_sys_clk       : std_logic;
signal c3_sys_rst_i       : std_logic;
signal c3_calib_done      : std_logic;
signal c3_clk0            : std_logic;
signal c3_rst0            : std_logic;

signal c3_p0_cmd_clk      : std_logic;
signal c3_p0_cmd_en       : std_logic;
signal c3_p0_cmd_instr    : std_logic_vector(2 downto 0);
signal c3_p0_cmd_bl       : std_logic_vector(5 downto 0);
signal c3_p0_cmd_byte_addr: std_logic_vector(29 downto 0):=(others=>'0');
signal c3_p0_cmd_empty    : std_logic;
signal c3_p0_cmd_full     : std_logic;
signal c3_p0_wr_clk       : std_logic;
signal c3_p0_wr_en        : std_logic;
signal c3_p0_wr_mask      : std_logic_vector(C3_P1_MASK_SIZE - 1 downto 0);
signal c3_p0_wr_data      : std_logic_vector(C3_P1_DATA_PORT_SIZE - 1 downto 0);
signal c3_p0_wr_full      : std_logic;
signal c3_p0_wr_empty     : std_logic;
signal c3_p0_wr_count     : std_logic_vector(6 downto 0);
signal c3_p0_wr_underrun  : std_logic;
signal c3_p0_wr_error     : std_logic;
signal c3_p0_rd_clk       : std_logic;
signal c3_p0_rd_en        : std_logic;
signal c3_p0_rd_data      : std_logic_vector(C3_P1_DATA_PORT_SIZE - 1 downto 0);
signal c3_p0_rd_full      : std_logic;
signal c3_p0_rd_empty     : std_logic;
signal c3_p0_rd_count     : std_logic_vector(6 downto 0);
signal c3_p0_rd_overflow  : std_logic;
signal c3_p0_rd_error     : std_logic;

signal c3_p1_cmd_clk      : std_logic;
signal c3_p1_cmd_en       : std_logic;
signal c3_p1_cmd_instr    : std_logic_vector(2 downto 0);
signal c3_p1_cmd_bl       : std_logic_vector(5 downto 0);
signal c3_p1_cmd_byte_addr: std_logic_vector(29 downto 0):=(others=>'0');
signal c3_p1_cmd_empty    : std_logic;
signal c3_p1_cmd_full     : std_logic;
signal c3_p1_wr_clk       : std_logic;
signal c3_p1_wr_en        : std_logic;
signal c3_p1_wr_mask      : std_logic_vector(C3_P1_MASK_SIZE - 1 downto 0);
signal c3_p1_wr_data      : std_logic_vector(C3_P1_DATA_PORT_SIZE - 1 downto 0);
signal c3_p1_wr_full      : std_logic;
signal c3_p1_wr_empty     : std_logic;
signal c3_p1_wr_count     : std_logic_vector(6 downto 0);
signal c3_p1_wr_underrun  : std_logic;
signal c3_p1_wr_error     : std_logic;
signal c3_p1_rd_clk       : std_logic;
signal c3_p1_rd_en        : std_logic;
signal c3_p1_rd_data      : std_logic_vector(C3_P1_DATA_PORT_SIZE - 1 downto 0);
signal c3_p1_rd_full      : std_logic;
signal c3_p1_rd_empty     : std_logic;
signal c3_p1_rd_count     : std_logic_vector(6 downto 0);
signal c3_p1_rd_overflow  : std_logic;
signal c3_p1_rd_error     : std_logic;

----------------------------------------------------
   
   signal vga_en    : std_logic := '1';
   signal vga_de    : std_logic := '0';
	signal vga_reg_hsync: STD_LOGIC :='1';
	signal vga_reg_vsync: STD_LOGIC :='1';
   signal vga_x     : std_logic_vector(9 downto 0) := (others => '0');
   signal vga_y     : std_logic_vector(9 downto 0) := (others => '0');
   signal vga_dirty_x: std_logic_vector(9 downto 0) := (others => '0');
   signal vga_dirty_y: std_logic_vector(9 downto 0) := (others => '0');	
   signal vga_pixel : std_logic_vector(15 downto 0) := (others => '0');	
   signal vga_char_x: std_logic_vector(6 downto 0) := (others => '0');
	signal vga_char_y: std_logic_vector(4 downto 0) := (others => '0');
   signal vga_char  : std_logic_vector(7 downto 0);
   signal vga_scanline_wea   : std_logic_vector(0 downto 0);
   signal vga_scanline_wr_addr: std_logic_vector(7 downto 0);
   signal vga_scanline_wr_data: std_logic_vector(63 downto 0);
   signal vga_scanline_pixel : std_logic_vector(15 downto 0) := (others => '0');	
   signal vga_ddr_rd_req : std_logic:='0';
   signal vga_ddr_rd_process : std_logic:='0';
   
	signal lcd_en    : std_logic := '1';
	signal lcd_de    : std_logic :='0';
	signal lcd_reg_hsync: STD_LOGIC :='1';
	signal lcd_reg_vsync: STD_LOGIC :='1';
   signal lcd_x     : std_logic_vector(9 downto 0) := (others => '0');
   signal lcd_y     : std_logic_vector(9 downto 0) := (others => '0');
   signal lcd_dirty_x: std_logic_vector(9 downto 0) := (others => '0');
   signal lcd_dirty_y: std_logic_vector(9 downto 0) := (others => '0');	
   signal lcd_pixel : std_logic_vector(23 downto 0) := (others => '0');	
	signal lcd_char_x: std_logic_vector(6 downto 0) := (others => '0');
	signal lcd_char_y: std_logic_vector(4 downto 0) := (others => '0');
   signal lcd_char  : std_logic_vector(7 downto 0);
   signal lcd_scanline_wea   : std_logic_vector(0 downto 0);
   signal lcd_scanline_wr_addr: std_logic_vector(7 downto 0);
   signal lcd_scanline_wr_data: std_logic_vector(63 downto 0);
   signal lcd_scanline_pixel : std_logic_vector(15 downto 0) := (others => '0');	
   signal lcd_ddr_rd_req : std_logic:='0';
   signal lcd_ddr_rd_process : std_logic:='0';

   signal cam_en       : std_logic := '1';
   signal cam_pixel_ready: std_logic := '0';
   signal cam_y        : std_logic_vector(9 downto 0):=(others=>'0');
   signal cam_x        : std_logic_vector(9 downto 0):=(others=>'0');
   signal cam_pixel    : std_logic_vector(15 downto 0):=(others=>'0');
   signal cam_scanline_wr_data  : std_logic_vector(15 downto 0):=(others=>'0');
   signal cam_scanline_rd_addr  : std_logic_vector(7 downto 0):=(others=>'0');
   signal cam_scanline_rd_data : std_logic_vector(63 downto 0):=(others=>'0');
   signal cam_ddr_wr_req : std_logic:='0';
   
   signal lcd_clk      : std_logic:='0';
	signal cam_clk      : std_logic:='0';
   signal vga_clk      : std_logic:='0';

   signal clk100      : std_logic:='0';
	signal clk50       : std_logic:='0';
	signal clk25       : std_logic:='0';
	signal clk10       : std_logic:='0';
   
begin
   clocking_chip : clocking_core
   PORT MAP (
      CLK_in => c3_clk0,
      CLK_100 =>CLK100,
      CLK_50 => CLK50,
      CLK_25 => CLK25,
      CLK_10 => CLK10
      );

	cam_clk<=clk25;
	lcd_clk<=clk10;
	lcd_dclk<=lcd_clk;

   vga_clk<=clk25;
---------------------------------------------------
   
   led(0)<=btn(0);
   led(1)<=btn(1);
   led(2)<=btn(2);
   led(3)<=btn(3);
   
   vga_en<=btn(3);
   lcd_en<=btn(2);
   cam_en<=btn(1);
   
   vga_hsync<=vga_reg_hsync;
   vga_vsync<=vga_reg_vsync;
   lcd_hsync<=lcd_reg_hsync;
   lcd_vsync<=lcd_reg_vsync;
-----------------------------------------------------

   lcd_scanline : DDR_LCD_scanline
   PORT MAP (
    clka  => c3_clk0,
    wea   => lcd_scanline_wea,
    addra => lcd_scanline_wr_addr,
    dina  => lcd_scanline_wr_data,
    clkb  => lcd_clk,
    addrb => lcd_x,
    doutb => lcd_scanline_pixel
   );
   
   lcd_pixel(15 downto 0)<=(others=>'1') when (lcd_x=240) or (lcd_y=136)
   else lcd_scanline_pixel;
   
   lcd_pixel(23 downto 16)<=(others=>'0');
   lcd_AN430_chip: lcd_AN430 PORT MAP(
      en    => lcd_en,
		clk   => lcd_clk,
		red   => lcd_red,
		green => lcd_green,
		blue  => lcd_blue,
		hsync => lcd_reg_hsync,
		vsync => lcd_reg_vsync,
		de	   => lcd_de,
		x     => lcd_x,
		y     => lcd_y,
      dirty_x=>lcd_dirty_x,
      dirty_y=>lcd_dirty_y,
      pixel => lcd_pixel,
		char_x=> lcd_char_x,
		char_y=> lcd_char_y,
		char_code  => lcd_char
      );
-----------------------------------------------------

   vga_scanline : DDR_LCD_scanline
   PORT MAP (
    clka  => c3_clk0,
    wea   => vga_scanline_wea,
    addra => vga_scanline_wr_addr,
    dina  => vga_scanline_wr_data,
    clkb  => vga_clk,
    addrb => vga_x,
    doutb => vga_scanline_pixel
   );
   
   vga_pixel(15 downto 0)<=(others=>'1') when (vga_x=320) or (vga_y=240)
   else vga_scanline_pixel;
   
	vga_640x480_chip: vga PORT MAP(
      en    => vga_en,
		clk   => vga_clk,
		red   => vga_red,
		green => vga_green,
		blue  => vga_blue,
		hsync => vga_reg_hsync,
		vsync => vga_reg_vsync,
      de    => vga_de,
		x     => vga_x,
		y     => vga_y,
      dirty_x=>vga_dirty_x,
      dirty_y=>vga_dirty_y,
		pixel => vga_pixel,
		char_x=> vga_char_x,
		char_y=> vga_char_y,
		char_code  => vga_char
      );
-----------------------------------------------------

   cam_scanline_wr_data<=cam_pixel;
   
   cam_scanline: CAM_DDR_scanline
   PORT MAP (
    clka  => cam_clk,
    wea   => (0=>cam_pixel_ready),
    addra => cam_x,
    dina  => cam_scanline_wr_data,
    clkb  => c3_clk0,
    addrb => cam_scanline_rd_addr,
    doutb => cam_scanline_rd_data
   );

--   --minimal OV7670 grayscale mode
   OV7670_PWDN  <= '0'; --0 - power on
   OV7670_RESET <= '1'; --0 -activate reset
   OV7670_XCLK  <= cam_clk;
   ov7670_siod  <= 'Z';
   ov7670_sioc  <= '0';
   
   capture: ov7670_capture PORT MAP(
      en    => cam_en,
		clk   => OV7670_PCLK,
		vsync => OV7670_VSYNC,
		href  => OV7670_HREF,
		din    => OV7670_D,
      cam_x =>cam_x,
      cam_y =>cam_y,
      pixel =>cam_pixel,
		ready =>cam_pixel_ready
      );
--------------------------------------------------------------------
   
   c3_sys_clk   <= clk50_ucf;
   c3_sys_rst_i <= reset_n;
   
   c3_p0_cmd_clk <= c3_clk0;
   c3_p0_rd_clk  <= c3_clk0;
   c3_p0_wr_clk  <= c3_clk0;
   
   c3_p1_cmd_clk <= c3_clk0;
   c3_p1_rd_clk  <= c3_clk0;
   c3_p1_wr_clk  <= c3_clk0;
   
  mig_ddr3_inst : mig_ddr3
    generic map (
    C3_P0_MASK_SIZE => C3_P0_MASK_SIZE,
    C3_P0_DATA_PORT_SIZE => C3_P0_DATA_PORT_SIZE,
    C3_P1_MASK_SIZE => C3_P1_MASK_SIZE,
    C3_P1_DATA_PORT_SIZE => C3_P1_DATA_PORT_SIZE,
    C3_MEMCLK_PERIOD => C3_MEMCLK_PERIOD,
    C3_RST_ACT_LOW => C3_RST_ACT_LOW,
    C3_INPUT_CLK_TYPE => C3_INPUT_CLK_TYPE,
    C3_CALIB_SOFT_IP => C3_CALIB_SOFT_IP,
    C3_SIMULATION => C3_SIMULATION,
    DEBUG_EN => DEBUG_EN,
    C3_MEM_ADDR_ORDER => C3_MEM_ADDR_ORDER,
    C3_NUM_DQ_PINS => C3_NUM_DQ_PINS,
    C3_MEM_ADDR_WIDTH => C3_MEM_ADDR_WIDTH,
    C3_MEM_BANKADDR_WIDTH => C3_MEM_BANKADDR_WIDTH
)
    port map (

  c3_sys_clk   => c3_sys_clk,
  c3_sys_rst_i => c3_sys_rst_i,                        

  c3_clk0      => c3_clk0,
  c3_rst0      => c3_rst0,
  c3_calib_done=> c3_calib_done,

  mcb3_dram_dq       => mcb3_dram_dq,  
  mcb3_dram_a        => mcb3_dram_a,  
  mcb3_dram_ba       => mcb3_dram_ba,
  mcb3_dram_ras_n    => mcb3_dram_ras_n,                        
  mcb3_dram_cas_n    => mcb3_dram_cas_n,                        
  mcb3_dram_we_n     => mcb3_dram_we_n,                          
  mcb3_dram_odt      => mcb3_dram_odt,
  mcb3_dram_cke      => mcb3_dram_cke,                          
  mcb3_dram_ck       => mcb3_dram_ck,                          
  mcb3_dram_ck_n     => mcb3_dram_ck_n,       
  mcb3_dram_dqs      => mcb3_dram_dqs,                          
  mcb3_dram_dqs_n    => mcb3_dram_dqs_n,
  mcb3_dram_reset_n  => mcb3_dram_reset_n,
  mcb3_dram_udqs     => mcb3_dram_udqs,    -- for X16 parts           
  mcb3_dram_udqs_n   => mcb3_dram_udqs_n,  -- for X16 parts
  mcb3_dram_udm      => mcb3_dram_udm,     -- for X16 parts
  mcb3_dram_dm       => mcb3_dram_dm,

  mcb3_rzq  => mcb3_rzq,
  mcb3_zio  => mcb3_zio,

  c3_p0_cmd_clk         => c3_p0_cmd_clk,
  c3_p0_cmd_en          => c3_p0_cmd_en,
  c3_p0_cmd_instr       => c3_p0_cmd_instr,
  c3_p0_cmd_bl          => c3_p0_cmd_bl,
  c3_p0_cmd_byte_addr   => c3_p0_cmd_byte_addr,
  c3_p0_cmd_empty       => c3_p0_cmd_empty,
  c3_p0_cmd_full        => c3_p0_cmd_full,
  c3_p0_wr_clk          => c3_p0_wr_clk,
  c3_p0_wr_en           => c3_p0_wr_en,
  c3_p0_wr_mask         => c3_p0_wr_mask,
  c3_p0_wr_data         => c3_p0_wr_data,
  c3_p0_wr_full         => c3_p0_wr_full,
  c3_p0_wr_empty        => c3_p0_wr_empty,
  c3_p0_wr_count        => c3_p0_wr_count,
  c3_p0_wr_underrun     => c3_p0_wr_underrun,
  c3_p0_wr_error        => c3_p0_wr_error,
  c3_p0_rd_clk          => c3_p0_rd_clk,
  c3_p0_rd_en           => c3_p0_rd_en,
  c3_p0_rd_data         => c3_p0_rd_data,
  c3_p0_rd_full         => c3_p0_rd_full,
  c3_p0_rd_empty        => c3_p0_rd_empty,
  c3_p0_rd_count        => c3_p0_rd_count,
  c3_p0_rd_overflow     => c3_p0_rd_overflow,
  c3_p0_rd_error        => c3_p0_rd_error,

  c3_p1_cmd_clk         => c3_p1_cmd_clk,
  c3_p1_cmd_en          => c3_p1_cmd_en,
  c3_p1_cmd_instr       => c3_p1_cmd_instr,
  c3_p1_cmd_bl          => c3_p1_cmd_bl,
  c3_p1_cmd_byte_addr   => c3_p1_cmd_byte_addr,
  c3_p1_cmd_empty       => c3_p1_cmd_empty,
  c3_p1_cmd_full        => c3_p1_cmd_full,
  c3_p1_wr_clk          => c3_p1_wr_clk,
  c3_p1_wr_en           => c3_p1_wr_en,
  c3_p1_wr_mask         => c3_p1_wr_mask,
  c3_p1_wr_data         => c3_p1_wr_data,
  c3_p1_wr_full         => c3_p1_wr_full,
  c3_p1_wr_empty        => c3_p1_wr_empty,
  c3_p1_wr_count        => c3_p1_wr_count,
  c3_p1_wr_underrun     => c3_p1_wr_underrun,
  c3_p1_wr_error        => c3_p1_wr_error,
  c3_p1_rd_clk          => c3_p1_rd_clk,
  c3_p1_rd_en           => c3_p1_rd_en,
  c3_p1_rd_data         => c3_p1_rd_data,
  c3_p1_rd_full         => c3_p1_rd_full,
  c3_p1_rd_empty        => c3_p1_rd_empty,
  c3_p1_rd_count        => c3_p1_rd_count,
  c3_p1_rd_overflow     => c3_p1_rd_overflow,
  c3_p1_rd_error        => c3_p1_rd_error
);
-----------------------------------------------------------------------


-----------------------------------------------------------------------

lcd_scanline_wr_addr<=c3_p1_cmd_byte_addr(10 downto 3)
                    when lcd_ddr_rd_process='1'
                    else (others=>'0');
                     
vga_scanline_wr_addr<=c3_p1_cmd_byte_addr(10 downto 3)
                    when vga_ddr_rd_process='1'
                    else (others=>'0');
-----------------------------------------------------------------------

video_ddr_read_proc:
   process (c3_clk0)
   variable fsm_ddr_read: natural range 0 to 7 :=0;
   begin
      if rising_edge(c3_clk0) then
         if vga_dirty_x=1 then vga_ddr_rd_req<='1'; end if;
         if lcd_dirty_x=1 then lcd_ddr_rd_req<='1'; end if;
         
         if (c3_rst0='1')or(c3_calib_done='0') then
            c3_p1_rd_en<='0';
            c3_p1_wr_en<='0';
            c3_p1_cmd_en<='0';
            c3_p1_cmd_instr<="000";
            c3_p1_cmd_bl<=(others=>'0');
            c3_p1_cmd_byte_addr<=(others=>'0');
         else
            case fsm_ddr_read is
            -- idle
            when 0 => 
               if vga_ddr_rd_req='1' then
                  c3_p1_cmd_byte_addr(29 downto 21)<=(others=>'0');
                  c3_p1_cmd_byte_addr(20 downto 11)<=vga_y;
                  c3_p1_cmd_byte_addr(10 downto 0)<=(others=>'0');
                  lcd_ddr_rd_process<='0'; vga_ddr_rd_process<='1';
                  fsm_ddr_read:=1;
               end if;
               if lcd_ddr_rd_req='1' then
                  c3_p1_cmd_byte_addr(29 downto 21)<=(others=>'0');
                  c3_p1_cmd_byte_addr(20 downto 11)<=lcd_y;
                  c3_p1_cmd_byte_addr(10 downto 0)<=(others=>'0');
                  lcd_ddr_rd_process<='1'; vga_ddr_rd_process<='0';
                  fsm_ddr_read:=1;
               end if;
            -- read request to DDR chip
            when 1 => 
               c3_p1_rd_en<='1';
               c3_p1_cmd_en<='1';
               c3_p1_cmd_instr<="001";
               c3_p1_cmd_bl<=(others=>'0');
               fsm_ddr_read:=2;
            -- DDR chip read timeout
            when 2 =>
               c3_p1_cmd_instr<="000";
               c3_p1_cmd_en<='0';
               c3_p1_rd_en<='0';
               fsm_ddr_read:=3;
            -- data transmit from DDR chip to video scanline
            when 3 => 
               if vga_ddr_rd_process='1' then
                  vga_scanline_wea<="1";
                  vga_scanline_wr_data<=c3_p1_rd_data;
               end if;
               if lcd_ddr_rd_process='1' then
                  lcd_scanline_wea<="1";
                  lcd_scanline_wr_data<=c3_p1_rd_data;
               end if;
               fsm_ddr_read:=4;
            -- prepare to read content of next 8 bytes
            when 4 =>
               lcd_scanline_wea<="0";
               vga_scanline_wea<="0";
               c3_p1_cmd_byte_addr(10 downto 3)<=c3_p1_cmd_byte_addr(10 downto 3)+1;
               fsm_ddr_read:=5;
            -- check to exit from cycle
            when 5 =>
               if ((vga_ddr_rd_process='1')and
                  (c3_p1_cmd_byte_addr(10 downto 1)=640))
                  or
                  ((lcd_ddr_rd_process='1')and
                  (c3_p1_cmd_byte_addr(10 downto 1)=480))
               then 
                  fsm_ddr_read:=0;
                  if vga_ddr_rd_process='1' then vga_ddr_rd_req<='0'; end if;
                  if lcd_ddr_rd_process='1' then lcd_ddr_rd_req<='0'; end if;
               else fsm_ddr_read:=1;
               end if;
            when others => null;
            end case;
         end if;
      end if;
   end process;

-----------------------------------------------------------------------
cam_scanline_rd_addr<=c3_p0_cmd_byte_addr(10 downto 3);

cam_ddr_write_proc:
   process (c3_clk0)
   variable fsm_ddr_write: natural range 0 to 7 :=0;
   begin
      if rising_edge(c3_clk0) then
         if cam_x=639 then cam_ddr_wr_req<='1'; end if;
         
         if (c3_rst0='1')or(c3_calib_done='0') then
            c3_p0_rd_en<='0';
            c3_p0_wr_en<='0';
            c3_p0_cmd_en<='0';
            c3_p0_cmd_instr<="000";
            c3_p0_cmd_bl<=(others=>'0');
            c3_p0_cmd_byte_addr<=(others=>'0');
            c3_p0_wr_mask<=(others=>'0');
            c3_p0_wr_data<=(others=>'0');
         else
            case fsm_ddr_write is
            -- idle
            when 0 => 
               if cam_ddr_wr_req='1' then
                  c3_p0_cmd_byte_addr(29 downto 21)<=(others=>'0');
                  c3_p0_cmd_byte_addr(20 downto 11)<=cam_y;
                  c3_p0_cmd_byte_addr(10 downto 0)<=(others=>'0');
                  fsm_ddr_write:=1;
               end if;
            -- read data from CAM scanline
            when 1 =>
               c3_p0_wr_data<=cam_scanline_rd_data;
               fsm_ddr_write:=2;
            -- write request to DDR chip
            when 2 =>
               c3_p0_wr_en<='1';
               c3_p0_cmd_en<='1';
               c3_p0_cmd_instr<="010";
               c3_p0_cmd_bl<=(others=>'0');
               fsm_ddr_write:=3;
            -- DDR chip write timeout
            when 3 =>
               c3_p0_cmd_instr<="000";
               c3_p0_wr_en<='0';
               c3_p0_cmd_en<='0';
               fsm_ddr_write:=4;
            -- prepare to write content of next 8 bytes
            when 4 =>
               c3_p0_cmd_byte_addr(10 downto 3)<=c3_p0_cmd_byte_addr(10 downto 3)+1;
               fsm_ddr_write:=5;
            -- check to exit from cycle
            when 5 =>
               if c3_p0_cmd_byte_addr(10 downto 1)=640
                  then fsm_ddr_write:=0; cam_ddr_wr_req<='0';
                  else fsm_ddr_write:=1;
               end if;
            when others => null;
            end case;
         end if;
      end if;
   end process;

end Behavioral;
