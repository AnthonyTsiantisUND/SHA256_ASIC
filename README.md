# SHA256 ASIC
## Final Project for CSE 30342 - Digital Integrated Circuits
### Developed by Anthony Tsiantis, Natalie Sekerak and Olivia Zino

## Project Overview
This project implements a Verilog design of the SHA256 Encryption algorithm tailored for an Application-Specific Integrated Circuit (ASIC). The primary goal is to take an input message and generate a 256-bit cryptographic hash effciently. 

### Key Feautures
* Input Handling - processes input byte-by-byte and pads for compatability, 
* Message Scheduling - transformms a 512-bit block into 64 32-bit words
* Compression Engine - performs the hashing operations 
* Constants - use the first 32 bits of the fractional parts of cube roots of the first 64 prime numbers
* Output Generation - produces the final hash in 16-bit segments 

### Inputs
* `clock` -  system clock signal
* `reset` -  resets the internal state
* `load_enable` -  signals the start of input loading
* `input_complete` - indicates that input data is fully loaded
* `input_data` - 8-bit input data stream

### Outputs
* `hashed_data` - 16-bit segments of the final hashed output

### Functions
#### Bitwise Operations
* Rotation - used in hash functions `σ0`, `σ1`, `Σ0`, `Σ1`
* Boolean operations - used in `choose` and `majority`
    * AND `&`
    * XOR `^`
    * NOT `&`

#### Padding 
Steps Include: 
1. Append a `1` bit
2. Add `0` bits until the total message length is 448 bits
3. Append the message length to reach 64 bits

#### Message Scheduling 
The first 16 words are directly from the padded message. All subsequent words are computed using: `W[t] = σ1(W[t−2]) + W[t−7] + σ0(W[t−15]) + W[t−16]`

#### Compression 
Each round updates the variables `A` `B` `C` `D` `E` `F` `G` `H` using: 
    * temporary variables
    * boolean functions
    * constants

### Finite State Machine Representation 

![IMG_5004](https://github.com/user-attachments/assets/892beab8-84c3-4bba-963d-575122c50a0a)

![IMG_2012](https://github.com/user-attachments/assets/abbc6258-6fd1-46a0-baa8-7df7e9660140)
