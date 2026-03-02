-- ============================================================================
-- I2C-style FSM Controller — inspired by GRLIB OpenCores I2C
-- Exercises: complex FSM, qualified expressions, type casts, with...select
-- ============================================================================

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity i2c_fsm is
    port (
        clk      : in  std_logic;
        rst      : in  std_logic;
        -- control
        start    : in  std_logic;    -- initiate transfer
        stop     : in  std_logic;    -- request stop
        wr_data  : in  std_logic_vector(7 downto 0);  -- byte to send
        -- status
        rd_data  : out std_logic_vector(7 downto 0);  -- received byte
        busy     : out std_logic;
        done     : out std_logic;
        -- serial lines
        scl_out  : out std_logic;
        sda_out  : out std_logic;
        sda_in   : in  std_logic
    );
end entity i2c_fsm;

architecture rtl of i2c_fsm is
    type state_t is (ST_IDLE, ST_START, ST_DATA, ST_ACK, ST_STOP, ST_DONE);
    signal state     : state_t;
    signal bit_cnt   : unsigned(3 downto 0);
    signal shift_reg : std_logic_vector(7 downto 0);
    signal rx_reg    : std_logic_vector(7 downto 0);
    signal clk_div   : unsigned(3 downto 0);
    signal scl_en    : std_logic;
    signal phase     : std_logic_vector(1 downto 0);
begin

    -- Clock divider for SCL generation
    clk_gen: process(clk)
    begin
        if rising_edge(clk) then
            if rst = '1' then
                clk_div <= (others => '0');
                phase   <= "00";
            else
                clk_div <= clk_div + 1;
                if clk_div = "1111" then
                    phase <= std_logic_vector(unsigned(phase) + 1);
                end if;
            end if;
        end if;
    end process clk_gen;

    -- SCL output: high when idle, toggled during transfer
    with state select
        scl_out <= '1'        when ST_IDLE,
                   '0'        when ST_START,
                   phase(1)   when ST_DATA,
                   phase(1)   when ST_ACK,
                   '1'        when ST_STOP,
                   '1'        when others;

    -- Main FSM
    fsm: process(clk)
    begin
        if rising_edge(clk) then
            if rst = '1' then
                state     <= ST_IDLE;
                bit_cnt   <= (others => '0');
                shift_reg <= (others => '0');
                rx_reg    <= (others => '0');
                sda_out   <= '1';
                busy      <= '0';
                done      <= '0';
            else
                done <= '0';
                case state is
                    when ST_IDLE =>
                        sda_out <= '1';
                        busy    <= '0';
                        if start = '1' then
                            state     <= ST_START;
                            shift_reg <= wr_data;
                            busy      <= '1';
                        end if;

                    when ST_START =>
                        -- Start condition: SDA goes low while SCL is high
                        sda_out <= '0';
                        bit_cnt <= to_unsigned(7, 4);
                        state   <= ST_DATA;

                    when ST_DATA =>
                        -- Shift out MSB first
                        sda_out <= shift_reg(7);
                        if phase = "11" and clk_div = "1111" then
                            -- Sample incoming bit on SCL rising edge
                            rx_reg <= rx_reg(6 downto 0) & sda_in;
                            shift_reg <= shift_reg(6 downto 0) & '0';
                            if bit_cnt = "0000" then
                                state <= ST_ACK;
                            else
                                bit_cnt <= bit_cnt - 1;
                            end if;
                        end if;

                    when ST_ACK =>
                        sda_out <= '1';  -- release for ACK
                        if phase = "11" and clk_div = "1111" then
                            if stop = '1' then
                                state <= ST_STOP;
                            else
                                state <= ST_DONE;
                            end if;
                        end if;

                    when ST_STOP =>
                        -- Stop condition: SDA goes high while SCL is high
                        sda_out <= '0';
                        if phase = "10" then
                            sda_out <= '1';
                            state   <= ST_DONE;
                        end if;

                    when ST_DONE =>
                        done  <= '1';
                        busy  <= '0';
                        state <= ST_IDLE;

                    when others =>
                        state <= ST_IDLE;
                end case;
            end if;
        end if;
    end process fsm;

    -- Output received data
    rd_data <= rx_reg;

end architecture rtl;
