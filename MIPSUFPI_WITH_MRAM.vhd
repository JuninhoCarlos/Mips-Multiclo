LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_ARITH.ALL;
USE IEEE.STD_LOGIC_UNSIGNED.ALL;
--USE IEEE.NUMERIC_STD.ALL;
LIBRARY altera_mf;
USE altera_mf.altera_mf_components.ALL;

ENTITY MIPSUFPI_WITH_MRAM IS
	PORT(
		clock, reset 			: IN STD_LOGIC; 
		program_counter_out 		: OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
		data_out 			: OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
		memory_data_register_out	: OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
		memory_address_register_out 	: OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
		memory_write_out 		: OUT STD_LOGIC 
	);
END MIPSUFPI_WITH_MRAM;

ARCHITECTURE a OF MIPSUFPI_WITH_MRAM IS
	TYPE BANKREG is array(0 to 31) of STD_LOGIC_VECTOR(31 DOWNTO 0);
	TYPE STATE_TYPE IS (reset_pc, fetch, decode, execute_add, execute_sub, execute_Lw, execute_Lw_2, execute_Sw, execute_Sw_2, execute_jump,
								execute_addu, execute_subu, execute_addi, execute_addiu,execute_mult, execute_div, execute_divu, execute_ld,
							   execute_ld_2, execute_ld_3, execute_lh, execute_lh_2, execute_lb, execute_lb_2, execute_lbu, execute_lbu_2, 			
                        execute_lhu, execute_lhu_2, execute_sd, execute_sd_2, execute_sd_3, execute_sh, execute_sh_2, execute_sb,
								execute_sb_2, execute_lui, execute_mfhi, execute_mflo,execute_mfc, execute_mtc, execute_and, execute_andi,
								execute_or, execute_ori, execute_xor, execute_nor, execute_slt, execute_slti, execute_sll, execute_srl,
								execute_sra,execute_beq, execute_beq_2, execute_bne, execute_bne_2, execute_jr, execute_jal,
								execute_sb_3, execute_sb_4, execute_sh_3, execute_sh_4,execute_blez,execute_blez_2,execute_bltz,execute_bgez
								,execute_bltz_2,execute_bgez_2);
								
	SIGNAL bank_register			: BANKREG;
--	SIGNAL control_register			: BANKREG;
	SIGNAL state					: STATE_TYPE;
	SIGNAL instruction_register		: STD_LOGIC_VECTOR(31 DOWNTO 0);
	SIGNAL memory_data_register 	: STD_LOGIC_VECTOR(31 DOWNTO 0);
	SIGNAL MIPS_data_out 			: STD_LOGIC_VECTOR(31 DOWNTO 0);
	SIGNAL program_counter,instruction_memory_address 			: STD_LOGIC_VECTOR(31 DOWNTO 0); 
	SIGNAL memory_address_register	: STD_LOGIC_VECTOR(31 DOWNTO 0);
	SIGNAL memory_write 			: STD_LOGIC;
	SIGNAL Reg_A					: STD_LOGIC_VECTOR(31 DOWNTO 0);
	SIGNAL Reg_B					: STD_LOGIC_VECTOR(31 DOWNTO 0);
	SIGNAL Reg_HILO					: STD_LOGIC_VECTOR(63 DOWNTO 0);
	SIGNAL Reg_HI					: STD_LOGIC_VECTOR(31 DOWNTO 0);
	SIGNAL Reg_LO					: STD_LOGIC_VECTOR(31 DOWNTO 0);
	SIGNAL ImmSinExt				: STD_LOGIC_VECTOR(31 DOWNTO 0);
	SIGNAL ImmZeroExt				: STD_LOGIC_VECTOR(31 DOWNTO 0);
	SIGNAL EndBranch				: STD_LOGIC_VECTOR(31 DOWNTO 0);
    SIGNAL Immediate_jump			: STD_LOGIC_VECTOR(31 DOWNTO 0);
	SIGNAL Address_Lw_Sw			: STD_LOGIC_VECTOR(31 DOWNTO 0);
	SIGNAL Overflow_trap			: STD_LOGIC_VECTOR(31 DOWNTO 0);
	SIGNAL shift_direction			: STD_LOGIC;
	SIGNAL shift_result				: STD_LOGIC_VECTOR (31 DOWNTO 0);
	SIGNAL arithshift_result		: STD_LOGIC_VECTOR (31 DOWNTO 0);
	SIGNAL my_quotient				: STD_LOGIC_VECTOR (31 DOWNTO 0);
	SIGNAL my_remain				: STD_LOGIC_VECTOR (31 DOWNTO 0);
	SIGNAL instruction_memory_q		: STD_LOGIC_VECTOR (31 DOWNTO 0);

COMPONENT MRAM
	PORT
	(
		address	: IN STD_LOGIC_VECTOR (13 DOWNTO 0);
		clock	: IN STD_LOGIC;
		data	: IN STD_LOGIC_VECTOR (31 DOWNTO 0);
		wren	: IN STD_LOGIC;
		q		: OUT STD_LOGIC_VECTOR (31 DOWNTO 0)
	);
END COMPONENT;

component ROM
	PORT
	(
		address		: IN STD_LOGIC_VECTOR (11 DOWNTO 0);
		clock		: IN STD_LOGIC  := '1';
		q		: OUT STD_LOGIC_VECTOR (31 DOWNTO 0)
	);
end component;


COMPONENT SHIFTER
	PORT
	(
		data		: IN STD_LOGIC_VECTOR (31 DOWNTO 0);
		direction	: IN STD_LOGIC;
		distance	: IN STD_LOGIC_VECTOR (4 DOWNTO 0);
		result		: OUT STD_LOGIC_VECTOR (31 DOWNTO 0)
	);
END COMPONENT;

COMPONENT ARITHSHIFTER
	PORT
	(
		data		: IN STD_LOGIC_VECTOR (31 DOWNTO 0);
		distance	: IN STD_LOGIC_VECTOR (4 DOWNTO 0);
		result		: OUT STD_LOGIC_VECTOR (31 DOWNTO 0)
	);
END COMPONENT;
	
COMPONENT DIVIDER
	PORT
	(
		denom		: IN STD_LOGIC_VECTOR (31 DOWNTO 0);
		numer		: IN STD_LOGIC_VECTOR (31 DOWNTO 0);
		quotient	: OUT STD_LOGIC_VECTOR (31 DOWNTO 0);
		remain		: OUT STD_LOGIC_VECTOR (31 DOWNTO 0)
	);
END COMPONENT;


	
BEGIN
	data_memory: MRAM
		PORT MAP (
			address => memory_address_register(13 DOWNTO 0),
			clock 	=> clock,
			data 	=> MIPS_data_out,
			wren 	=> memory_write, 
			q 		=> memory_data_register
		);
	
	instructions_memory: ROM 
		PORT MAP (
			address	 => instruction_memory_address(11 DOWNTO 0),
			clock	 => clock,
			q	 => instruction_memory_q
		); 
		
	my_shifter: SHIFTER
		PORT MAP (
			data 		=> Reg_B(31 DOWNTO 0),
			direction	=> shift_direction,
			distance 	=> instruction_register(10 DOWNTO 6),
			result		=> shift_result
	);
	
	my_arithshifter: ARITHSHIFTER
		PORT MAP (
			data		=> Reg_B(31 DOWNTO 0),
			distance	=> instruction_register(10 DOWNTO 6),
			result		=> arithshift_result
	);
	
	my_divider: DIVIDER
		PORT MAP (
			denom		=> Reg_B(31 DOWNTO 0),
			numer		=> Reg_A(31 DOWNTO 0),
			quotient	=> my_quotient(31 DOWNTO 0),
			remain		=> my_remain(31 DOWNTO 0)
	);
		
	program_counter_out 				<= program_counter;
	data_out 							<= MIPS_data_out;
	memory_data_register_out 		<= memory_data_register;
	memory_address_register_out 	<= memory_address_register;		
	memory_write_out 					<= memory_write;

	ImmSinExt(31 DOWNTO 0) 		<= conv_std_Logic_vector(-conv_integer(instruction_register(15)),16)&instruction_register(15 DOWNTO 0);
	EndBranch(31 DOWNTO 0) 		<= conv_std_Logic_vector(-conv_integer(instruction_register(15)),14)&instruction_register(15 DOWNTO 0)&"00";
	ImmZeroExt(31 DOWNTO 0) 	<= "0000000000000000"&instruction_register(15 DOWNTO 0);
	Immediate_jump(31 DOWNTO 0) <= program_counter(31 DOWNTO 28)&instruction_register(25 DOWNTO 0)&"00";

	
	PROCESS (clock, reset)
		BEGIN
		IF reset = '1' THEN
			state <= reset_pc;
		ELSIF (clock'EVENT AND clock = '1') THEN  
			CASE state IS
				WHEN reset_pc =>
					program_counter 		<= "00000000000000000000000000000000";
					bank_register 			<= (others => (others => '0'));
					Address_Lw_Sw			<= (others => '0');
					state 					<= fetch;
			
				WHEN fetch =>
					instruction_register 		<= instruction_memory_q;
					program_counter 		<= program_counter + 4;
					state 				<= decode;
			
				WHEN decode =>
				
					Reg_A(31 DOWNTO 0) 		<= bank_register(conv_integer(unsigned(instruction_register(25 DOWNTO 21))))(31 DOWNTO 0);
					Reg_B(31 DOWNTO 0) 		<= bank_register(conv_integer(unsigned(instruction_register(20 DOWNTO 16))))(31 DOWNTO 0);
					Reg_HI(31 DOWNTO 0) 		<= Reg_HILO(63 DOWNTO 32);
					Reg_LO(31 DOWNTO 0) 		<= Reg_HILO(31 DOWNTO 0);
					Address_Lw_Sw(31 DOWNTO 0) 	<= bank_register(conv_integer(unsigned(instruction_register(25 DOWNTO 21))))(31 DOWNTO 0) + ImmSinExt(31 DOWNTO 0);
					IF (Instruction_register(31 DOWNTO 26)) = "000000" THEN
						CASE instruction_register(5 DOWNTO 0) IS 
							
							
							WHEN "100000" =>
								state <= execute_add;
							WHEN "100010" =>
								state <= execute_sub;
							WHEN "100001" =>
								state <= execute_addu;
							WHEN "100011" =>
								state <= execute_subu;
							WHEN "011000" =>
								state <= execute_mult;
							WHEN "011010" =>
								state <= execute_div;
							WHEN "011011" =>
								state <= execute_divu;
							WHEN "010000" =>
								state <= execute_mfhi;
							WHEN "010010" =>
								state <= execute_mflo;
--							WHEN "111110" =>
--								state <= execute_mfc;
--							WHEN "111111" =>
--								state <= execute_mtc;
							WHEN "100100" =>
								state <= execute_and;
							WHEN "100101" =>
								state <= execute_or;
							WHEN "100110" =>
								state <= execute_xor;
							WHEN "100111" =>
								state <= execute_nor;
							WHEN "101010" =>
								state <= execute_slt;
							WHEN "000000" =>
								state <= execute_sll;
								shift_direction 			<= '0';
							WHEN "000010" =>
								state <= execute_srl;
								shift_direction 			<= '1';
							WHEN "000011" =>
								state <= execute_sra;
							WHEN "001000" =>
								state <= execute_jr;
							WHEN OTHERS =>
								state <= fetch;
						END CASE;
					ELSE
						CASE instruction_register(31 DOWNTO 26) IS 
						
							WHEN "000001" =>--1 
								if conv_integer(instruction_register(20 downto 16)) = 0 then
									state <= execute_bltz;
								elsif conv_integer(instruction_register(20 downto 16)) = 1 then
									state <= execute_bgez;
								end if;
								
							WHEN "000010" =>
								state <= execute_jump;
							WHEN "000011" =>
								state <= execute_jal;
							WHEN "000100" =>
								state <= execute_beq;
							WHEN "000101" =>
								state <= execute_bne;
							WHEN "001000" =>
								state <= execute_addi;
							WHEN "001001" =>
								state <= execute_addiu;
							WHEN "001010" =>
								state <= execute_slti;
							WHEN "001100" =>
								state <= execute_andi;
							WHEN "001101" =>
								state <= execute_ori;
							WHEN "001111" =>
								state <= execute_lui;
							WHEN "100000" =>
								state <= execute_lb;
							WHEN "100001" =>
								state <= execute_lh;
							WHEN "100011" =>
								state <= execute_Lw;
							WHEN "100100" =>
								state <= execute_lbu;
							WHEN "100101" =>
								state <= execute_lhu;
							WHEN "101000" =>
								state <= execute_sb;
								MIPS_data_out(31 DOWNTO 0) <= bank_register(conv_integer(instruction_register(20 DOWNTO 16)))(31 DOWNTO 0);
							WHEN "101001" =>
								state <= execute_sh;
								MIPS_data_out(31 DOWNTO 0) <= bank_register(conv_integer(instruction_register(20 DOWNTO 16)))(31 DOWNTO 0);
							WHEN "101011" =>
								state <= execute_Sw;
								MIPS_data_out(31 DOWNTO 0) <= bank_register(conv_integer(instruction_register(20 DOWNTO 16)))(31 DOWNTO 0);
							WHEN "100111" =>
								state <= execute_ld;
							WHEN "101111" =>
								state <= execute_sd;
								MIPS_data_out(31 DOWNTO 0) <= bank_register(conv_integer(instruction_register(20 DOWNTO 16)))(31 DOWNTO 0);
							WHEN OTHERS =>
								state <= fetch;
						END CASE;
					END IF;

				WHEN execute_add =>
					IF (instruction_register(15 DOWNTO 11)) = "00000" THEN
						bank_register(0)(31 DOWNTO 0) <= "00000000000000000000000000000000";
					ELSIF (Reg_A(31 DOWNTO 0) + Reg_B(31 DOWNTO 0) > 1073741824) THEN
						Overflow_trap(31 DOWNTO 0) <= Reg_A(31 DOWNTO 0) + Reg_B(31 DOWNTO 0) - 1073741824;
						bank_register(conv_integer(instruction_register(15 DOWNTO 11)))(31 DOWNTO 0) <= "01111111111111111111111111111111";
					ELSIF (Reg_A(31 DOWNTO 0) + Reg_B(31 DOWNTO 0) < -1073741824) THEN
						Overflow_trap(31 DOWNTO 0) <= Reg_A(31 DOWNTO 0) + Reg_B(31 DOWNTO 0) + 1073741824;
						bank_register(conv_integer(instruction_register(15 DOWNTO 11)))(31 DOWNTO 0) <= "11111111111111111111111111111111";
					ELSE
						bank_register(conv_integer(instruction_register(15 DOWNTO 11)))(31 DOWNTO 0) <= Reg_A(31 DOWNTO 0) + Reg_B(31 DOWNTO 0);
					END IF;
					state <= fetch;
				
				WHEN execute_bgez =>
					IF (CONV_INTEGER(Reg_A(31 DOWNTO 0)) >= CONV_INTEGER(bank_register(0)(31 DOWNTO 0))) THEN
						state <= execute_bgez_2;
					ELSE
						state <= fetch;
					END IF;
					
				WHEN execute_bgez_2 =>
					program_counter <= program_counter + EndBranch(31 downto 0);
					state <= fetch;
				
				WHEN execute_bltz =>
					IF (CONV_INTEGER(Reg_A(31 DOWNTO 0)) < CONV_INTEGER(bank_register(0)(31 DOWNTO 0))) THEN
						state <= execute_bltz_2;
					ELSE
						state <= fetch;
					END IF;
					
				WHEN execute_bltz_2 =>
					
					program_counter <= program_counter + EndBranch(31 downto 0);
					state <= fetch;
				
				
				
				WHEN execute_blez =>
					IF (CONV_INTEGER(Reg_A(31 DOWNTO 0)) <= CONV_INTEGER(bank_register(0)(31 DOWNTO 0))) THEN
						state <= execute_blez_2;
					ELSE
						state <= fetch;
					END IF;
					
				WHEN execute_blez_2 =>
					program_counter <= program_counter + EndBranch(31 downto 0);
					state <= fetch;
					
				WHEN execute_addi =>
					IF (instruction_register(20 DOWNTO 16)) = "00000" THEN
						bank_register(0)(31 DOWNTO 0) <= "00000000000000000000000000000000";
					ELSIF (Reg_A(31 DOWNTO 0) + ImmSinExt(31 DOWNTO 0) > 2147483647) THEN
						Overflow_trap(31 DOWNTO 0) <= Reg_A(31 DOWNTO 0) + ImmSinExt(31 DOWNTO 0) - 2147483647;
						bank_register(conv_integer(instruction_register(20 DOWNTO 16)))(31 DOWNTO 0) <= "01111111111111111111111111111111";
					ELSIF (Reg_A(31 DOWNTO 0) + ImmSinExt(31 DOWNTO 0) < -2147483647) THEN
						Overflow_trap(31 DOWNTO 0) <= Reg_A(31 DOWNTO 0) + ImmSinExt(31 DOWNTO 0) + 2147483647;
						bank_register(conv_integer(instruction_register(20 DOWNTO 16)))(31 DOWNTO 0) <= "11111111111111111111111111111111";
					ELSE
						bank_register(conv_integer(instruction_register(20 DOWNTO 16)))(31 DOWNTO 0) <= Reg_A(31 DOWNTO 0) + ImmSinExt(31 DOWNTO 0);
					END IF;
					state <= fetch;
				
				WHEN execute_addiu =>
					IF (instruction_register(20 DOWNTO 16)) = "00000" THEN
						bank_register(0)(31 DOWNTO 0) <= "00000000000000000000000000000000";
					ELSE
						bank_register(conv_integer(instruction_register(20 DOWNTO 16)))(31 DOWNTO 0) <= Reg_A(31 DOWNTO 0) + ImmSinExt(31 DOWNTO 0);
					END IF;
					state <= fetch;
				
				WHEN execute_addu =>
					IF (instruction_register(15 DOWNTO 11)) = "00000" THEN
						bank_register(0)(31 DOWNTO 0) <= "00000000000000000000000000000000";
					ELSE
						bank_register(conv_integer(instruction_register(15 DOWNTO 11)))(31 DOWNTO 0) <= Reg_A(31 DOWNTO 0) + Reg_B(31 DOWNTO 0);
					END IF;
					state <= fetch;
				
				WHEN execute_subu =>
					IF (instruction_register(15 DOWNTO 11)) = "00000" THEN
						bank_register(0)(31 DOWNTO 0) <= "00000000000000000000000000000000";
					ELSE
						bank_register(conv_integer(instruction_register(15 DOWNTO 11)))(31 DOWNTO 0) <= Reg_A(31 DOWNTO 0) - Reg_B(31 DOWNTO 0);
					END IF;
					state <= fetch;

				WHEN execute_sub =>
					IF (instruction_register(15 DOWNTO 11)) = "00000" THEN
						bank_register(0)(31 DOWNTO 0) <= "00000000000000000000000000000000";
					ELSIF (Reg_A(31 DOWNTO 0) - Reg_B(31 DOWNTO 0) > 1073741824) THEN
						Overflow_trap(31 DOWNTO 0) <= Reg_A(31 DOWNTO 0) + Reg_B(31 DOWNTO 0) - 1073741824;
						bank_register(conv_integer(instruction_register(15 DOWNTO 11)))(31 DOWNTO 0) <= "01111111111111111111111111111111";
					ELSIF (Reg_A(31 DOWNTO 0) - Reg_B(31 DOWNTO 0) < -1073741824) THEN
						Overflow_trap(31 DOWNTO 0) <= Reg_A(31 DOWNTO 0) + Reg_B(31 DOWNTO 0) + 1073741824;
						bank_register(conv_integer(instruction_register(15 DOWNTO 11)))(31 DOWNTO 0) <= "11111111111111111111111111111111";
					ELSE
						bank_register(conv_integer(instruction_register(15 DOWNTO 11)))(31 DOWNTO 0) <= Reg_A(31 DOWNTO 0) - Reg_B(31 DOWNTO 0);
					END IF;
					state <= fetch;
				
				WHEN execute_mult =>
					Reg_HILO(63 DOWNTO 0) 	<= Reg_A(31 DOWNTO 0) * Reg_B(31 DOWNTO 0);
					state 			<= fetch;
			
				WHEN execute_div =>
					Reg_HILO(63 DOWNTO 32) <= my_remain(31 DOWNTO 0);
					Reg_HILO(31 DOWNTO 0) <= my_quotient(31 DOWNTO 0);
					state 				<= fetch;
			
				WHEN execute_divu =>
					Reg_HILO(63 DOWNTO 32) <= (conv_Std_logic_vector(((conv_integer(Reg_A(31 DOWNTO 0))) REM (conv_integer(Reg_B(31 DOWNTO 0)))),32));
					Reg_HILO(31 DOWNTO 0) <= (conv_Std_logic_vector(((conv_integer(Reg_A(31 DOWNTO 0))) / (conv_integer(Reg_B(31 DOWNTO 0)))),32));
					state 		    <= fetch;

				WHEN execute_mfhi =>
					IF (instruction_register(15 DOWNTO 11)) = "00000" THEN
						bank_register(0)(31 DOWNTO 0) <= "00000000000000000000000000000000";
					ELSE
						bank_register(conv_integer(instruction_register(15 DOWNTO 11)))(31 DOWNTO 0) <= Reg_HI(31 DOWNTO 0);
					END IF;
					state <= fetch;

				WHEN execute_mflo =>
					IF (instruction_register(15 DOWNTO 11)) = "00000" THEN
						bank_register(0)(31 DOWNTO 0) <= "00000000000000000000000000000000";
					ELSE
						bank_register(conv_integer(instruction_register(15 DOWNTO 11)))(31 DOWNTO 0) <= Reg_LO(31 DOWNTO 0);
					END IF;
					state <= fetch;
					
--CÓDIGO QUE SÓ SERÁ USADO QUANDO FOR IMPLANTADO NA ARQUITETURA MULTICORE
--				WHEN execute_mfc =>
--					IF (instruction_register(20 DOWNTO 16)) = "00000" THEN
--						bank_register(0)(31 DOWNTO 0) <= "00000000000000000000000000000000";
--					ELSE
--						bank_register(conv_integer(instruction_register(20 DOWNTO 16)))(31 DOWNTO 0) <= control_register(conv_integer(instruction_register(25 DOWNTO 21)))(31 DOWNTO 0);
--					END IF;
--					state <= fetch;
--
--
--				WHEN execute_mtc =>
--					control_register(conv_integer(instruction_register(25 DOWNTO 21)))(31 DOWNTO 0) <= bank_register(conv_integer(instruction_register(20 DOWNTO 16)))(31 DOWNTO 0);
--					state <= fetch;

				WHEN execute_Lw =>
					state <= execute_Lw_2;
				
				WHEN execute_Lw_2 =>
					IF (instruction_register(20 DOWNTO 16)) = "00000" THEN
						bank_register(0)(31 DOWNTO 0) <= "00000000000000000000000000000000";
					ELSE
						bank_register(conv_integer (instruction_register(20 DOWNTO 16)))(31 DOWNTO 0) <= memory_data_register;
					END IF;
					state <= fetch;

				WHEN execute_ld =>
					state <= execute_ld_2;
				
				WHEN execute_ld_2 =>
					IF (instruction_register(20 DOWNTO 16)) = "00000" THEN
						bank_register(0)(31 DOWNTO 0) <= "00000000000000000000000000000000";
					ELSE
						bank_register(conv_integer(instruction_register(20 DOWNTO 16)))(31 DOWNTO 0) <= memory_data_register;
					END IF;
					state <= execute_ld_3;
				
				WHEN execute_ld_3 =>
					IF (instruction_register(20 DOWNTO 16)) = "00000" THEN
						bank_register(0)(31 DOWNTO 0) <= "00000000000000000000000000000000";
					ELSE
						bank_register(conv_integer(instruction_register(20 DOWNTO 16)) + 1)(31 DOWNTO 0) <= memory_data_register;
					END IF;
					state <= fetch;

				WHEN execute_lh =>
					state <= execute_lh_2;

				WHEN execute_lh_2 =>
					IF (instruction_register(20 DOWNTO 16)) = "00000" THEN
						bank_register(0)(31 DOWNTO 0) <= "00000000000000000000000000000000";
					ELSE
						bank_register(conv_integer(instruction_register(20 DOWNTO 16)))(31 DOWNTO 0) <= conv_std_Logic_vector(-conv_integer(memory_data_register(15)),16)		
																      &memory_data_register(15 DOWNTO 0);
					END IF;
					state <= fetch;

				WHEN execute_lhu =>
					state <= execute_lhu_2;

				WHEN execute_lhu_2 =>
					IF (instruction_register(20 DOWNTO 16)) = "00000" THEN
						bank_register(0)(31 DOWNTO 0) <= "00000000000000000000000000000000";
					ELSE
						bank_register(conv_integer(instruction_register(20 DOWNTO 16)))(31 DOWNTO 0) <= "0000000000000000"&memory_data_register(15 DOWNTO 0);
					END IF;
					state <= fetch;

				WHEN execute_lb =>
					state <= execute_lb_2;

				WHEN execute_lb_2 =>
					IF (instruction_register(20 DOWNTO 16)) = "00000" THEN
						bank_register(0)(31 DOWNTO 0) <= "00000000000000000000000000000000";
					ELSE
						bank_register(conv_integer(instruction_register(20 DOWNTO 16)))(31 DOWNTO 0) <= conv_std_Logic_vector(conv_integer(memory_data_register(7)),24)&memory_data_register(7 DOWNTO 0);--Laysson: conv_integer era feito assim: -conv_integer
					END IF;
					state <= fetch;

				WHEN execute_lbu =>
					state <= execute_lbu_2;

				WHEN execute_lbu_2 =>
					IF (instruction_register(20 DOWNTO 16)) = "00000" THEN
						bank_register(0)(31 DOWNTO 0) <= "00000000000000000000000000000000";
					ELSE
						bank_register(conv_integer(instruction_register(20 DOWNTO 16)))(31 DOWNTO 0) <= "000000000000000000000000"&memory_data_register(7 DOWNTO 0);
					END IF;
					state <= fetch;

				WHEN execute_sd =>
					MIPS_data_out(31 DOWNTO 0) <= bank_register(conv_integer(instruction_register(20 DOWNTO 16)) + 1)(31 DOWNTO 0);
					state <= execute_sd_2;

				WHEN execute_sd_2 =>
					state <= execute_sd_3;

				WHEN execute_sd_3 =>
					state <= fetch;

				WHEN execute_sh =>
					state <= execute_sh_2;

				WHEN execute_sh_2 =>
					MIPS_data_out(31 DOWNTO 16) <= memory_data_register(31 DOWNTO 16);
					state <= execute_sh_3;

				WHEN execute_sh_3 =>
					state <= execute_sh_4;

				WHEN execute_sh_4 =>
					state <= fetch;

				WHEN execute_sb =>
					state <= execute_sb_2;

				WHEN execute_sb_2 =>
					MIPS_data_out(31 DOWNTO 8) <= memory_data_register(31 DOWNTO 8);
					state <= execute_sb_3;

				WHEN execute_sb_3 =>
					state <= execute_sb_4;

				WHEN execute_sb_4 =>
					state <= fetch;

				WHEN execute_Sw =>
					state <= execute_Sw_2;

				WHEN execute_Sw_2 =>
					state <= fetch;

				WHEN execute_lui =>
					IF (instruction_register(20 DOWNTO 16)) = "00000" THEN
						bank_register(0)(31 DOWNTO 0) <= "00000000000000000000000000000000";
					ELSE
						bank_register(conv_integer(instruction_register(20 DOWNTO 16)))(15 DOWNTO 0) <= (others => '0');
						bank_register(conv_integer(instruction_register(20 DOWNTO 16)))(31 DOWNTO 16) <= instruction_register(15 DOWNTO 0);
					END IF;
					state <= fetch;

				WHEN execute_and =>
					IF (instruction_register(15 DOWNTO 11)) = "00000" THEN
						bank_register(0)(31 DOWNTO 0) <= "00000000000000000000000000000000";
					ELSE
						bank_register(conv_integer(instruction_register(15 DOWNTO 11)))(31 DOWNTO 0) <= Reg_A(31 DOWNTO 0) AND Reg_B(31 DOWNTO 0);
					END IF;
					state <= fetch;

				WHEN execute_andi =>
					IF (instruction_register(20 DOWNTO 16)) = "00000" THEN
						bank_register(0)(31 DOWNTO 0) <= "00000000000000000000000000000000";
					ELSE
						bank_register(conv_integer(instruction_register(20 DOWNTO 16)))(31 DOWNTO 0) <= Reg_A(31 DOWNTO 0) AND ImmZeroExt(31 DOWNTO 0);
					END IF;
					state <= fetch;

				WHEN execute_or =>
					IF (instruction_register(15 DOWNTO 11)) = "00000" THEN
						bank_register(0)(31 DOWNTO 0) <= "00000000000000000000000000000000";
					ELSE
						bank_register(conv_integer(instruction_register(15 DOWNTO 11)))(31 DOWNTO 0) <= Reg_A(31 DOWNTO 0) OR Reg_B(31 DOWNTO 0);
					END IF;
					state <= fetch;

				WHEN execute_ori =>
					IF (instruction_register(20 DOWNTO 16)) = "00000" THEN
						bank_register(0)(31 DOWNTO 0) <= "00000000000000000000000000000000";
					ELSE
						bank_register(conv_integer(instruction_register(20 DOWNTO 16)))(31 DOWNTO 0) <= Reg_A(31 DOWNTO 0) OR ImmZeroExt(31 DOWNTO 0);
					END IF;
					state <= fetch;

				WHEN execute_xor =>
					IF (instruction_register(15 DOWNTO 11)) = "00000" THEN
						bank_register(0)(31 DOWNTO 0) <= "00000000000000000000000000000000";
					ELSE
						bank_register(conv_integer(instruction_register(15 DOWNTO 11)))(31 DOWNTO 0) <= Reg_A(31 DOWNTO 0) XOR Reg_B(31 DOWNTO 0);
					END IF;
					state <= fetch;

				WHEN execute_nor =>
					IF (instruction_register(15 DOWNTO 11)) = "00000" THEN
						bank_register(0)(31 DOWNTO 0) <= "00000000000000000000000000000000";
					ELSE
						bank_register(conv_integer(instruction_register(15 DOWNTO 11)))(31 DOWNTO 0) <= Reg_A(31 DOWNTO 0) NOR Reg_B(31 DOWNTO 0);
					END IF;
					state <= fetch;

				WHEN execute_slt =>
					IF (instruction_register(15 DOWNTO 11)) = "00000" THEN
						bank_register(0)(31 DOWNTO 0) <= "00000000000000000000000000000000";
					ELSE
						IF (conv_integer(Reg_A(31 DOWNTO 0)) < conv_integer(Reg_B(31 DOWNTO 0))) THEN
							bank_register(conv_integer(instruction_register(15 DOWNTO 11)))(31 DOWNTO 0) <= "00000000000000000000000000000001";
						ELSE
							bank_register(conv_integer(instruction_register(15 DOWNTO 11)))(31 DOWNTO 0) <= "00000000000000000000000000000000";
						END IF;
					END IF;
					state <= fetch;

				WHEN execute_slti =>
					IF (instruction_register(20 DOWNTO 16)) = "00000" THEN
						bank_register(0)(31 DOWNTO 0) <= "00000000000000000000000000000000";
					ELSE
						IF (conv_integer(Reg_A(31 DOWNTO 0)) < ImmSinExt(31 DOWNTO 0)) THEN
							bank_register(conv_integer(instruction_register(20 DOWNTO 16)))(31 DOWNTO 0) <= "00000000000000000000000000000001";
						ELSE
							bank_register(conv_integer(instruction_register(20 DOWNTO 16)))(31 DOWNTO 0) <= "00000000000000000000000000000000";
						END IF;
					END IF;
					state <= fetch;

				WHEN execute_sll =>
					IF (instruction_register(15 DOWNTO 11)) = "00000" THEN
						bank_register(0)(31 DOWNTO 0) <= "00000000000000000000000000000000";
					ELSE
						bank_register(conv_integer(instruction_register(15 DOWNTO 11)))(31 DOWNTO 0) <= shift_result(31 DOWNTO 0);
					END IF;
					state <= fetch;
					

				WHEN execute_srl =>
					IF (instruction_register(15 DOWNTO 11)) = "00000" THEN
						bank_register(0)(31 DOWNTO 0) <= "00000000000000000000000000000000";
					ELSE
						bank_register(conv_integer(instruction_register(15 DOWNTO 11)))(31 DOWNTO 0) <= shift_result(31 DOWNTO 0);
					END IF;
					state <= fetch;

				WHEN execute_sra =>
					IF (instruction_register(15 DOWNTO 11)) = "00000" THEN
						bank_register(0)(31 DOWNTO 0) <= "00000000000000000000000000000000";
					ELSE
						bank_register(conv_integer(instruction_register(15 DOWNTO 11)))(31 DOWNTO 0) <= arithshift_result(31 DOWNTO 0);
					END IF;
					state <= fetch;
									
				WHEN execute_beq =>
					IF Reg_A(31 DOWNTO 0) = Reg_B(31 DOWNTO 0) THEN
						state <= execute_beq_2;
					ELSE
						state <= fetch;
					END IF;

				WHEN execute_beq_2 =>
					program_counter <= program_counter + EndBranch(31 DOWNTO 0);
					state 		<= fetch;

				WHEN execute_bne =>
					IF Reg_A(31 DOWNTO 0) /= Reg_B(31 DOWNTO 0) THEN
						state <= execute_bne_2;
					ELSE
						state <= fetch;
					END IF;

				WHEN execute_bne_2 =>
					program_counter <= program_counter + EndBranch(31 DOWNTO 0);
					state 		<= fetch;

				WHEN execute_jump =>
					program_counter	<= Immediate_jump(31 DOWNTO 0);
					state 		<= fetch;

				WHEN execute_jr =>
					program_counter <= Reg_A(31 DOWNTO 0);
					state 		<= fetch;

				WHEN execute_jal =>
					bank_register(31)(31 DOWNTO 0) 	<= program_counter(31 DOWNTO 0);
					program_counter 		<= Immediate_jump(31 DOWNTO 0);
					state 				<= fetch;
				
				WHEN OTHERS =>					
					state <= fetch;
			END CASE;
		END IF;
	END PROCESS;
	
	WITH state SELECT
		memory_address_register <=	"00000000000000000000000000000000"	WHEN reset_pc,
						Address_Lw_Sw	 			WHEN execute_Lw,
						Address_Lw_Sw			 	WHEN execute_Sw,
						Address_Lw_Sw				WHEN execute_ld,
						Address_Lw_Sw + 1			WHEN execute_ld_2,
						Address_Lw_Sw + 1			WHEN execute_ld_3,
						Address_Lw_Sw				WHEN execute_lh,
						Address_Lw_Sw				WHEN execute_lbu,
						Address_Lw_Sw				WHEN execute_lhu,
						Address_Lw_Sw				WHEN execute_sd,
						Address_Lw_Sw + 1			WHEN execute_sd_2,
						Address_Lw_Sw				WHEN execute_sh,
						Address_Lw_Sw				WHEN execute_sh_3,
						Address_Lw_Sw				WHEN execute_sb,
						Address_Lw_Sw				WHEN execute_sb_3,
						program_counter				WHEN OTHERS;
						
	with state select
		instruction_memory_address <=
			Immediate_jump 				when execute_jal,
			Reg_A						WHEN execute_jr,
			Immediate_jump				WHEN execute_jump,
			program_counter + EndBranch	WHEN execute_beq_2,
			program_counter + EndBranch	WHEN execute_bne_2,
			program_counter + EndBranch	WHEN execute_blez_2,					
			program_counter + EndBranch	WHEN execute_bltz_2,
			program_counter + EndBranch	WHEN execute_bgez_2,
			program_counter 			when others;
	

	WITH state SELECT
		memory_write <= '1' WHEN execute_Sw,
				'1' WHEN execute_sd,
				'1' WHEN execute_sd_2,
				'1' WHEN execute_sh_3,
				'1' WHEN execute_sb_3,
				'0' WHEN OTHERS;
END ARCHITECTURE a;
