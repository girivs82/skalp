library ieee;
use ieee.std_logic_1164.all;

-- VHDL-2019: Interface declaration
interface simple_bus is
    signal data  : std_logic_vector(7 downto 0);
    signal valid : std_logic;
    signal ready : std_logic;
end interface simple_bus;

-- VHDL-2019: Master view (drives data/valid, receives ready)
view bus_master of simple_bus is
    data  : out;
    valid : out;
    ready : in;
end view bus_master;

-- VHDL-2019: Slave view (receives data/valid, drives ready)
view bus_slave of simple_bus is
    data  : in;
    valid : in;
    ready : out;
end view bus_slave;

-- Sender: puts tx_data on bus when triggered
entity sender is
    port (
        clk     : in  std_logic;
        rst     : in  std_logic;
        trigger : in  std_logic;
        tx_data : in  std_logic_vector(7 downto 0);
        bus     : view bus_master
    );
end entity sender;

architecture rtl of sender is
begin
    -- Combinational: drive bus when triggered
    bus_data  <= tx_data when trigger = '1' else (others => '0');
    bus_valid <= trigger;
end architecture rtl;

-- Receiver: latches data from bus when valid
entity receiver is
    port (
        clk      : in  std_logic;
        rst      : in  std_logic;
        bus      : view bus_slave;
        rx_data  : out std_logic_vector(7 downto 0);
        rx_valid : out std_logic
    );
end entity receiver;

architecture rtl of receiver is
begin
    bus_ready <= '1'; -- always ready

    process(clk)
    begin
        if rising_edge(clk) then
            if rst = '1' then
                rx_data  <= (others => '0');
                rx_valid <= '0';
            elsif bus_valid = '1' then
                rx_data  <= bus_data;
                rx_valid <= '1';
            else
                rx_valid <= '0';
            end if;
        end if;
    end process;
end architecture rtl;

-- Top-level: connects sender to receiver via internal bus signals
entity bus_system is
    port (
        clk      : in  std_logic;
        rst      : in  std_logic;
        trigger  : in  std_logic;
        tx_data  : in  std_logic_vector(7 downto 0);
        rx_data  : out std_logic_vector(7 downto 0);
        rx_valid : out std_logic
    );
end entity bus_system;

architecture rtl of bus_system is
    signal bus_data  : std_logic_vector(7 downto 0);
    signal bus_valid : std_logic;
    signal bus_ready : std_logic;
begin
    u_sender: entity work.sender
        port map (
            clk       => clk,
            rst       => rst,
            trigger   => trigger,
            tx_data   => tx_data,
            bus_data  => bus_data,
            bus_valid => bus_valid,
            bus_ready => bus_ready
        );

    u_receiver: entity work.receiver
        port map (
            clk       => clk,
            rst       => rst,
            bus_data  => bus_data,
            bus_valid => bus_valid,
            bus_ready => bus_ready,
            rx_data   => rx_data,
            rx_valid  => rx_valid
        );
end architecture rtl;
