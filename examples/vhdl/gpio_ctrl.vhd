-- ============================================================================
-- GPIO Controller — inspired by NEORV32 GPIO
-- Exercises: with...select, for-generate, named associations, case statement
-- ============================================================================

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity gpio_ctrl is
    generic (
        NUM_PINS : integer := 8
    );
    port (
        clk     : in  std_logic;
        rst     : in  std_logic;
        -- register interface
        addr    : in  std_logic_vector(1 downto 0);
        wdata   : in  std_logic_vector(7 downto 0);
        rdata   : out std_logic_vector(7 downto 0);
        we      : in  std_logic;
        -- GPIO pins
        gpio_in : in  std_logic_vector(7 downto 0);
        gpio_out: out std_logic_vector(7 downto 0);
        gpio_dir: out std_logic_vector(7 downto 0);
        -- interrupt
        irq     : out std_logic
    );
end entity gpio_ctrl;

architecture rtl of gpio_ctrl is
    signal out_reg   : std_logic_vector(7 downto 0);
    signal dir_reg   : std_logic_vector(7 downto 0);
    signal irq_en    : std_logic_vector(7 downto 0);
    signal in_sync   : std_logic_vector(7 downto 0);
    signal in_prev   : std_logic_vector(7 downto 0);
    signal irq_pend  : std_logic_vector(7 downto 0);
    signal read_mux  : std_logic_vector(7 downto 0);
begin

    -- Read mux using with...select (exercises Step 5)
    with addr select
        read_mux <= in_sync  when "00",
                    out_reg  when "01",
                    dir_reg  when "10",
                    irq_pend when others;

    rdata <= read_mux;

    -- Register write process
    reg_write: process(clk)
    begin
        if rising_edge(clk) then
            if rst = '1' then
                out_reg <= (others => '0');
                dir_reg <= (others => '0');
                irq_en  <= (others => '0');
            elsif we = '1' then
                case addr is
                    when "01"   => out_reg <= wdata;
                    when "10"   => dir_reg <= wdata;
                    when "11"   => irq_en  <= wdata;
                    when others => null;
                end case;
            end if;
        end if;
    end process reg_write;

    -- Input synchronizer (double-flop)
    sync: process(clk)
    begin
        if rising_edge(clk) then
            if rst = '1' then
                in_sync <= (others => '0');
                in_prev <= (others => '0');
            else
                in_sync <= gpio_in;
                in_prev <= in_sync;
            end if;
        end if;
    end process sync;

    -- Edge-detect IRQ: rising edge on any enabled pin
    irq_gen: process(clk)
    begin
        if rising_edge(clk) then
            if rst = '1' then
                irq_pend <= (others => '0');
            else
                irq_pend <= irq_pend or (irq_en and in_sync and (not in_prev));
            end if;
        end if;
    end process irq_gen;

    -- IRQ output: OR-reduce pending interrupts
    irq <= '1' when irq_pend /= "00000000" else '0';

    -- Output drivers
    gpio_out <= out_reg;
    gpio_dir <= dir_reg;

end architecture rtl;
