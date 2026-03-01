library ieee;
use ieee.std_logic_1164.all;

entity mux4 is
    port (
        a   : in  std_logic_vector(7 downto 0);
        b   : in  std_logic_vector(7 downto 0);
        c   : in  std_logic_vector(7 downto 0);
        d   : in  std_logic_vector(7 downto 0);
        sel : in  std_logic_vector(1 downto 0);
        y   : out std_logic_vector(7 downto 0)
    );
end entity mux4;

architecture rtl of mux4 is
begin

    process(all)
    begin
        case sel is
            when "00" =>
                y <= a;
            when "01" =>
                y <= b;
            when "10" =>
                y <= c;
            when others =>
                y <= d;
        end case;
    end process;

end architecture rtl;
