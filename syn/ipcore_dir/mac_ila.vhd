-------------------------------------------------------------------------------
-- Copyright (c) 2013 Xilinx, Inc.
-- All Rights Reserved
-------------------------------------------------------------------------------
--   ____  ____
--  /   /\/   /
-- /___/  \  /    Vendor     : Xilinx
-- \   \   \/     Version    : 14.4
--  \   \         Application: XILINX CORE Generator
--  /   /         Filename   : mac_ila.vhd
-- /___/   /\     Timestamp  : Fri Jan 25 17:36:39 GMT 2013
-- \   \  /  \
--  \___\/\___\
--
-- Design Name: VHDL Synthesis Wrapper
-------------------------------------------------------------------------------
-- This wrapper is used to integrate with Project Navigator and PlanAhead

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
ENTITY mac_ila IS
  port (
    CONTROL: inout std_logic_vector(35 downto 0);
    CLK: in std_logic;
    TRIG0: in std_logic_vector(21 downto 0);
    TRIG1: in std_logic_vector(38 downto 0);
    TRIG2: in std_logic_vector(37 downto 0));
END mac_ila;

ARCHITECTURE mac_ila_a OF mac_ila IS
BEGIN

END mac_ila_a;
