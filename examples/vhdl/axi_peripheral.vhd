library ieee;
use ieee.std_logic_1164.all;

interface axi_lite is
    signal awaddr  : std_logic_vector(31 downto 0);
    signal awvalid : std_logic;
    signal awready : std_logic;
    signal wdata   : std_logic_vector(31 downto 0);
    signal wvalid  : std_logic;
    signal wready  : std_logic;
end interface axi_lite;

view axi_master of axi_lite is
    awaddr  : out;
    awvalid : out;
    awready : in;
    wdata   : out;
    wvalid  : out;
    wready  : in;
end view axi_master;

entity axi_reg is
    port (
        clk : in std_logic;
        rst : in std_logic;
        bus : view axi_master
    );
end entity axi_reg;

architecture rtl of axi_reg is
    signal reg_data : std_logic_vector(31 downto 0);
begin
    -- bus_awready and bus_wready always asserted
    bus_awready <= '1';
    bus_wready  <= '1';

    process(clk)
    begin
        if rising_edge(clk) then
            if rst = '1' then
                reg_data <= (others => '0');
            elsif bus_awvalid = '1' and bus_wvalid = '1' then
                reg_data <= bus_wdata;
            end if;
        end if;
    end process;
end architecture rtl;
