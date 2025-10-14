use std::{collections::HashMap};
use crate::opcodes;

bitflags! {
    /// http://wiki.nesdev.com/w/index.php/Status_flags
    ///
    /// ```
    /// 7  bit  0
    /// ---- ----
    /// NV1B DIZC
    /// |||| ||||
    /// |||| |||+- Carry
    /// |||| ||+-- Zero
    /// |||| |+--- Interrupt Disable
    /// |||| +---- Decimal
    /// |||+------ (No CPU effect; see: the B flag)
    /// ||+------- (No CPU effect; always pushed as 1)
    /// |+-------- Overflow
    /// +--------- Negative
    /// ```
    ///
    pub struct CpuFlags: u8 {
        const CARRY             = 0b00000001;
        const ZERO              = 0b00000010;
        const INTERRUPT_DISABLE = 0b00000100;
        const DECIMAL_MODE      = 0b00001000;
        const BREAK             = 0b00010000;
        const UNUSED            = 0b00100000;
        const OVERFLOW          = 0b01000000;
        const NEGATIVE          = 0b10000000;
    }
}

const STACK: u16 = 0x0100;
const STACK_RESET: u8 = 0xfd;

pub struct CPU {
    pub register_a: u8,
    pub register_x: u8,
    pub register_y: u8,
    pub status: CpuFlags,
    pub program_counter: u16,
    pub stack_pointer: u8,
    memory: [u8; 0xFFFF]
}

#[derive(Debug)]
#[allow(non_camel_case_types)]
pub enum AddressingMode {
   Immediate,
   ZeroPage,
   ZeroPage_X,
   ZeroPage_Y,
   Absolute,
   Absolute_X,
   Absolute_Y,
   Indirect_X,
   Indirect_Y,
   NoneAddressing,
}

impl CPU {
    pub fn new() -> Self {
        CPU {
            register_a: 0,
            register_x: 0,
            register_y: 0,
            status: CpuFlags::from_bits_truncate(0b100100),
            program_counter: 0,
            stack_pointer: STACK_RESET,
            memory: [0; 0xFFFF]
        }
    }

    fn get_operand_address(&self, mode: &AddressingMode) -> u16 {
        match mode {
            AddressingMode::Immediate => self.program_counter,
            
            AddressingMode::ZeroPage => self.mem_read(self.program_counter) as u16,
            AddressingMode::ZeroPage_X => self.mem_read(self.program_counter).wrapping_add(self.register_x) as u16,
            AddressingMode::ZeroPage_Y => self.mem_read(self.program_counter).wrapping_add(self.register_y) as u16,
            
            AddressingMode::Absolute => self.mem_read_u16(self.program_counter),
            AddressingMode::Absolute_X => self.mem_read_u16(self.program_counter).wrapping_add(self.register_x as u16),
            AddressingMode::Absolute_Y => self.mem_read_u16(self.program_counter).wrapping_add(self.register_y as u16),
            
            AddressingMode::Indirect_X => {
                let base = self.mem_read(self.program_counter);
                
                let ptr: u8 = (base as u8).wrapping_add(self.register_x);
                
                let lo = self.mem_read(ptr as u16);
                let hi = self.mem_read(ptr.wrapping_add(1) as u16);
                (hi as u16) << 8 | (lo as u16)
            }
            AddressingMode::Indirect_Y => {
                let base = self.mem_read(self.program_counter);
                
                let lo = self.mem_read(base as u16);
                let hi = self.mem_read((base as u8).wrapping_add(1) as u16);
                
                let deref_base = (hi as u16) << 8 | (lo as u16);
                deref_base.wrapping_add(self.register_y as u16)
            }
            
            AddressingMode::NoneAddressing => panic!("mode {:?} is not supported", mode)
        }
    }

    fn mem_read(&self, addr: u16) -> u8 {
        self.memory[addr as usize]
    }

    fn mem_write(&mut self, addr: u16, data: u8) {
        self.memory[addr as usize] = data;
    }

    fn mem_read_u16(&self, pos: u16) -> u16 {
        let lo = self.mem_read(pos) as u16;
        let hi = self.mem_read(pos.wrapping_add(1)) as u16;
        (hi << 8) | (lo as u16)
    }

    fn mem_write_u16(&mut self, pos: u16, data: u16) {
        let hi = (data >> 8) as u8;
        let lo = (data & 0xFF) as u8;
        self.mem_write(pos, lo);
        self.mem_write(pos.wrapping_add(1), hi);
    }

    pub fn reset(&mut self) {
        self.register_a = 0;
        self.register_x = 0;
        self.register_y = 0;
        self.stack_pointer = STACK_RESET;
        self.status = CpuFlags::from_bits_truncate(0b100100);
        self.program_counter = self.mem_read_u16(0xFFFC);
    }

    pub fn load(&mut self, program: Vec<u8>) {
        self.memory[0x8000..(0x8000 + program.len())].copy_from_slice(&program[..]);
        self.mem_write_u16(0xFFFC, 0x8000);
    }

    pub fn load_and_run(&mut self, program: Vec<u8>) {
        self.load(program);
        self.reset();
        self.run()
    }

    fn update_zero_and_negative_flags(&mut self, result: u8) {
        if result == 0 {
            self.status.insert(CpuFlags::ZERO);
        } else {
            self.status.remove(CpuFlags::ZERO);
        }

        if result & 0b1000_0000 != 0 {
            self.status.insert(CpuFlags::NEGATIVE);
        } else {
            self.status.remove(CpuFlags::NEGATIVE);
        }
    }

    fn set_register_a(&mut self, result: u8) {
        self.register_a = result;
        self.update_zero_and_negative_flags(result);
    }

    fn set_register_x(&mut self, result: u8) {
        self.register_x = result;
        self.update_zero_and_negative_flags(result);
    }

    fn set_register_y(&mut self, result: u8) {
        self.register_y = result;
        self.update_zero_and_negative_flags(result);
    }

    /// ignoring decimal mode
    /// http://www.righto.com/2012/12/the-6502-overflow-flag-explained.html
    fn add_to_register_a(&mut self, value: u8) {
        let sum = self.register_a as u16 
            + value as u16
            + (if self.status.contains(CpuFlags::CARRY) {
                1
            } else {
                0
            }) as u16;

        if sum > 0xFF {
            self.status.insert(CpuFlags::CARRY);
        } else {
            self.status.remove(CpuFlags::CARRY);
        }

        let result = sum as u8;

        if (value ^ result) & (self.register_a ^ result) & 0x80 != 0 {
            self.status.insert(CpuFlags::OVERFLOW);
        } else {
            self.status.remove(CpuFlags::OVERFLOW);
        }

        self.set_register_a(result);
    }

    pub fn run(&mut self) {
        let ref opcodes: HashMap<u8, &'static opcodes::OpCode> = *opcodes::OPCODES_MAP;

        loop {
            let code = &self.mem_read(self.program_counter);
            self.program_counter += 1;
            let program_counter_state = self.program_counter;

            let opcode = opcodes.get(code).expect(&format!("OpCode {:x} is not recognized", code));
            let mode = &opcode.mode;
            let mnemonic = opcode.mnemonic;

            match mnemonic {
                /* Access */
                "LDA" => {
                    let addr = self.get_operand_address(mode);
                    let value = self.mem_read(addr);
        
                    self.set_register_a(value);
                }
                "STA" => {
                    let addr = self.get_operand_address(mode);

                    self.mem_write(addr, self.register_a);
                }
                "LDX" => {
                    let addr = self.get_operand_address(mode);
                    let value = self.mem_read(addr);
        
                    self.set_register_x(value);
                }
                "STX" => {
                    let addr = self.get_operand_address(mode);

                    self.mem_write(addr, self.register_x);
                }
                "LDY" => {
                    let addr = self.get_operand_address(mode);
                    let value = self.mem_read(addr);
        
                    self.set_register_y(value);
                }
                "STY" => {
                    let addr = self.get_operand_address(mode);

                    self.mem_write(addr, self.register_y);
                },

                /* Transfer */
                "TAX" => {
                    self.set_register_x(self.register_a);
                }
                "TXA" => {
                    self.set_register_a(self.register_x);
                }
                "TAY" => {
                    self.set_register_y(self.register_a);
                }
                "TYA" => {
                    self.set_register_a(self.register_y);
                }

                /* Arithmetic */
                "ADC" => {
                    let addr = self.get_operand_address(mode);
                    let value = self.mem_read(addr);

                    self.add_to_register_a(value);
                }
                "SBC" => {
                    let addr = self.get_operand_address(mode);
                    let value = self.mem_read(addr);

                    self.add_to_register_a(((value as i8).wrapping_neg().wrapping_sub(1)) as u8);
                }
                "INC" => {
                    todo!()
                }
                "DEC" => {
                    todo!()
                }
                "INX" => {
                    self.set_register_x(self.register_x.wrapping_add(1));
                }
                "DEX" => {
                    self.set_register_x(self.register_x.wrapping_sub(1));
                }
                "INY" => {
                    self.set_register_y(self.register_y.wrapping_add(1));
                }
                "DEY" => {
                    self.set_register_x(self.register_x.wrapping_sub(1));
                }

                // /* Shift */
                // ASL 	LSR 	ROL 	ROR 	

                // /* Bitwise */
                // AND 	ORA 	EOR 	BIT 				

                // /* Compare */
                // CMP 	CPX 	CPY 		

                // /* Branch */
                // BCC 	BCS 	BEQ 	BNE 	BPL 	BMI 	BVC 	BVS

                // /* Jump */
                // JMP 	JSR 	RTS 		
                "BRK" => {
                    return
                }	
                // RTI

                // /* Stack */
                // PHA 	PLA 	PHP 	PLP 	TXS 	TSX 		

                // /* Flags */
                // CLC 	SEC 	CLI 	SEI 	CLD 	SED 	CLV 	

                /* Other */
                "NOP" => {
                    
                }

                _=> {
                    panic!();
                }
            }

            if program_counter_state == self.program_counter {
                self.program_counter += (opcode.len - 1) as u16;
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_0xa9_lda_immediate_load_data() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xA9, 0x05, 0x00]);
        
        assert_eq!(cpu.register_a, 5);
        assert!(!cpu.status.contains(CpuFlags::ZERO));
        assert!(!cpu.status.contains(CpuFlags::NEGATIVE));
    }

    #[test]
    fn test_0xa9_lda_zero_flag() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xA9, 0x00, 0x00]);
        
        assert!(cpu.status.contains(CpuFlags::ZERO));
    }

    #[test]
    fn test_0xa9_lda_negative_flag() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xA9, 0xFF, 0x00]);
        
        assert!(cpu.status.contains(CpuFlags::NEGATIVE));
    }

    #[test]
    fn test_lda_from_memory() {
       let mut cpu = CPU::new();
       cpu.mem_write(0x10, 0x55);

       cpu.load_and_run(vec![0xA5, 0x10, 0x00]);

       assert_eq!(cpu.register_a, 0x55);
    }

   #[test]
    fn test_0xaa_tax_move_a_to_x() {
       let mut cpu = CPU::new();
       cpu.load_and_run(vec![0xA9, 0x0A, 0xAA, 0x00]);
 
       assert_eq!(cpu.register_x, 10)
    }

    #[test]
    fn test_0xaa_tax_zero_flag() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xA9, 0x00, 0xAA, 0x00]);

        assert!(cpu.status.contains(CpuFlags::ZERO));
    }

    #[test]
    fn test_0xaa_tax_negative_flag() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xA9, 0xFF, 0xAA, 0x00]);
        
        assert!(cpu.status.contains(CpuFlags::NEGATIVE));
    }
    
       #[test]
   fn test_5_ops_working_together() {
       let mut cpu = CPU::new();
       cpu.load_and_run(vec![0xA9, 0xC0, 0xAA, 0xE8, 0x00]);
 
       assert_eq!(cpu.register_x, 0xc1)
   }

    #[test]
    fn test_inx_overflow() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xA9, 0xFF, 0xAA, 0xE8, 0xE8, 0x00]);

        assert_eq!(cpu.register_x, 1)
    }

    #[test]
    fn test_adc_overflow() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xA9, 0x50, 0x69, 0x50, 0x00]);

        assert_eq!(cpu.register_a, 0xA0);
        assert!(cpu.status.contains(CpuFlags::OVERFLOW));
    }
}