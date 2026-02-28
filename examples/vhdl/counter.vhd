library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity counter is
    port (
        clk   : in  std_logic;
        rst   : in  std_logic;
        en    : in  std_logic;
        count : out unsigned(7 downto 0)
    );
end entity counter;

architecture rtl of counter is
    signal count_reg : unsigned(7 downto 0);
begin

    process(clk)
    begin
        if rising_edge(clk) then
            if rst = '1' then
                count_reg <= (others => '0');
            elsif en = '1' then
                count_reg <= count_reg + 1;
            end if;
        end if;
    end process;

    count <= count_reg;

end architecture rtl;
