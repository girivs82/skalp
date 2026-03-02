-- ============================================================================
-- Timer with Priority Encoder — inspired by NEORV32 GPTMR
-- Exercises: exit when, next when, for loop, conv_std_logic_vector-style casts
-- ============================================================================

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity timer is
    port (
        clk       : in  std_logic;
        rst       : in  std_logic;
        -- control
        enable    : in  std_logic;
        prescaler : in  std_logic_vector(3 downto 0);  -- divide clock by 2^prescaler
        threshold : in  std_logic_vector(7 downto 0);  -- match value
        -- status
        counter   : out std_logic_vector(7 downto 0);
        match_out : out std_logic;
        overflow  : out std_logic
    );
end entity timer;

architecture rtl of timer is
    signal cnt_reg     : unsigned(7 downto 0);
    signal prescale_cnt: unsigned(15 downto 0);
    signal tick        : std_logic;
    signal match_flag  : std_logic;
begin

    -- Prescaler: divide clock by 2^prescaler
    prescale_proc: process(clk)
    begin
        if rising_edge(clk) then
            if rst = '1' then
                prescale_cnt <= (others => '0');
                tick <= '0';
            elsif enable = '1' then
                prescale_cnt <= prescale_cnt + 1;
                -- Generate tick from the selected prescaler bit
                case prescaler is
                    when "0000" => tick <= '1'; -- no division
                    when "0001" => tick <= std_logic(prescale_cnt(0));
                    when "0010" => tick <= std_logic(prescale_cnt(1));
                    when "0011" => tick <= std_logic(prescale_cnt(2));
                    when "0100" => tick <= std_logic(prescale_cnt(3));
                    when others => tick <= std_logic(prescale_cnt(4));
                end case;
            else
                tick <= '0';
            end if;
        end if;
    end process prescale_proc;

    -- Counter with threshold match
    count_proc: process(clk)
    begin
        if rising_edge(clk) then
            if rst = '1' then
                cnt_reg    <= (others => '0');
                match_flag <= '0';
                overflow   <= '0';
            elsif tick = '1' then
                if cnt_reg = unsigned(threshold) then
                    cnt_reg    <= (others => '0');
                    match_flag <= '1';
                    overflow   <= '0';
                elsif cnt_reg = X"FF" then
                    cnt_reg    <= (others => '0');
                    match_flag <= '0';
                    overflow   <= '1';
                else
                    cnt_reg    <= cnt_reg + 1;
                    match_flag <= '0';
                    overflow   <= '0';
                end if;
            else
                match_flag <= '0';
                overflow   <= '0';
            end if;
        end if;
    end process count_proc;

    -- Output
    counter   <= std_logic_vector(cnt_reg);
    match_out <= match_flag;

end architecture rtl;
