// Javascript PDP 11/70 Emulator v1.5
// written by Paul Nankervis
// Please send suggestions, fixes and feedback to paulnank@hotmail.com
// I'm particularly interested in hearing from anyone with real experience on a PDP 11/70 front panel
//
// This code may be used freely provided the original author name is acknowledged in any modified source code
//
// http://skn.noip.me/pdp11/pdp11.html
//
//
//
//
const IOBASE_VIRT = 0160000;
const IOBASE_18BIT = 0760000;
const IOBASE_UNIBUS = 017000000;
const IOBASE_22BIT = 017760000;
const MAX_MEMORY = IOBASE_UNIBUS - 16384; // Maximum memory address (need less memory for BSD 2.9 boot)
const MAX_ADDRESS = 020000000; // Special register addresses are above 22 bit addressing

const BYTE_MODE = 1;
const READ_MODE = 2;
const WRITE_MODE = 4;
const MODIFY_WORD = READ_MODE | WRITE_MODE;
const MODIFY_BYTE = READ_MODE | WRITE_MODE | BYTE_MODE;


var STATE = {
    RUN: 0,
	RESET: 1,
    WAIT: 2,
    HALT: 3
}; // CPU.runState is either STATE.RUN, STATE.WAIT or STATE.HALT

var CPU = {
    controlReg: [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], // various control registers we don't really care about
    CPU_Error: 0,
    cpuType: 70,
    displayAddress: 0, // Address display for console operations
    displayPhysical: 0, // Physical address display for console operations
    displayRegister: 0, // Console display lights register
    flagC: 0x10000, // PSW C bit
    flagN: 0x8000, // PSW N bit
    flagV: 0x8000, // PSW V bit
    flagZ: 0xffff, // ~ PSW Z bit
    memory: [], // Main memory (in words)
    MMR0: 0, // MMU control registers
    MMR1: 0,
    MMR2: 0,
    MMR3: 0,
    MMR3Map: [4, 2, 0, 1], // map from mode to MMR3 I/D bit mask
    mmuEnable: 0, // MMU enable mask for READ_MODE or WRITE_MODE
    mmuLastMode: 0,
    mmuLastPage: 0,
    mmuMode: 0, // current memory management mode (0=kernel,1=super,2=undefined,3=user)
    mmuPAR: [
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], //kernel 0
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], //super 1
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], // mode 2 (not used)
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] //user 3
    ], // memory management PAR registers by mode
    mmuPDR: [
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], //kernel 0
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], //super 1
        [0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff], // mode 2 with illegal PDRs
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] //user 3
    ], // memory management PDR registers by mode
    PIR: 0, // Programmable interrupt register
    priorityReview: 1, // flag to mark if we need to check priority change
    PSW: 0xf, // PSW less flags C, N, V & Z
    registerAlt: [0, 0, 0, 0, 0, 0], // Alternate registers R0 - R5
    registerVal: [0, 0, 0, 0, 0, 0, 0, 0], // Current registers  R0 - R7
    stackLimit: 0xff, // stack overflow limit
    stackPointer: [0, 0, 0, 0], // Alternate R6 (kernel, super, illegal, user)
    switchRegister: 0, // console switch register
    trapMask: 0, // Mask of traps to be taken at the end of the current instruction
    trapPSW: -1, // PSW when first trap invoked - for tacking double traps
    unibusMap: [
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    ], // 32 unibus map registers
    runState: -1, // current machine state defined in STATE
    interruptQueue: [], // List of interrupts pending
};


var log = {
    depth: 64, // 256,
    extras: 0,
    history: []
};


function LOG_INSTRUCTION(instruction, format, name) {
    if (log.depth > 0) {
        if (log.history.length > log.depth + 4) {
            ////LOG_PRINT();
            //log.history = [];
            log.history.splice(0, 5);
        }
        log.history.push([readPSW(), CPU.registerVal[7], instruction, format, name, -1, -1]);
    }
}


function LOG_SOURCE(source) {
    if (log.depth > 0) {
        if (log.history.length > 0) {
            if (log.history[log.history.length - 1][5] < 0) {
                log.history[log.history.length - 1][5] = source;
            } else {
                log.history[log.history.length - 1][6] = source;
            }
        }
    }
}


function LOG_ADDRESS(address) {
    if (log.depth > 0) {
        if (log.history.length > 0) {
            log.history[log.history.length - 1].push(address);
        }
    }
}


function LOG_OPERAND(operand, history, index) {
    var result = "R" + (operand & 7).toString();
    switch ((operand >> 3) & 7) {
        case 1:
            result = "(" + result + ")";
            break;
        case 2:
            if ((operand & 7) === 7 && history[5 + index] >= 0) {
                result = "#" + history[5 + index].toString(8);
            } else {
                result = "(" + result + ")+";
            }
            break;
        case 3:
            if ((operand & 7) === 7) {
                result = "@#" + history[log.extras++].toString(8);
            } else {
                result = "@" + result;
            }
            break;
        case 4:
            result = "-(" + result + ")";
            break;
        case 5:
            result = "@-(" + result + ")";
            break;
        case 6:
            if ((operand & 7) === 7) {
                result = history[log.extras++].toString(8);
            } else {
                result = history[log.extras++].toString(8) + "(" + result + ")";
            }
            break;
        case 7:
            if ((operand & 7) === 7) {
                result = "@" + history[log.extras++].toString(8);
            } else {
                result = "@" + history[log.extras++].toString(8) + "(" + result + ")";
            }
            break;
    }
    return result;
}


function LOG_OCTAL(n) {
    n = n.toString(8);
    return "000000".substr(n.length) + n;
}


function LOG_PRINT() {
    var i, result, psw, pc, instruction, name, historyRec;
    for (i = 0; i < log.history.length; i++) {
        log.extras = 7;
        historyRec = log.history[i];
        psw = historyRec[0];
        pc = (historyRec[1] - 2) & 0xffff;
        instruction = historyRec[2];
        name = historyRec[4];
        result = LOG_OCTAL(psw) + " " + LOG_OCTAL(pc) + " " + name;
        switch (historyRec[3]) {
            case 0: // No operands - just print name
                break;
            case 1: // One operand instructions
                result += " " + LOG_OPERAND(instruction, historyRec, 0);
                break;
            case 2: // Two operand instructions
                result += " " + LOG_OPERAND(instruction >> 6, historyRec, 0) + "," + LOG_OPERAND(instruction, historyRec, 1);
                break;
            case 3: // Instruction with a register and a normal operand
                result += " " + LOG_OPERAND((instruction >> 6) & 7, [], 0) + "," + LOG_OPERAND(instruction, historyRec, 0);
                break;
            case 4: // Branch instructions
                result += " " + branch(historyRec[1], instruction).toString(8);
                break;
            case 5: // SOB instruction
                result += " " + LOG_OPERAND((instruction >> 6) & 7, [], 0) + "," + (historyRec[1] - (((instruction & 077) << 1)) & 0xffff).toString(8);
                break;
            case 6: // Single register instruction (RTS)
                result += " " + LOG_OPERAND(instruction & 7, [], 0);
                break;
            case 7: // Eight bit number instruction (EMT & TRAP)
                result += " " + (instruction & 0xff).toString(8);
                break;
            case 8: // Six bit number instruction (MARK)
                result += " " + (instruction & 0x3f).toString(8);
                break;
            case 9: // Three bit number instruction (SPL)
                result += " " + (instruction & 0x7).toString(8);
                break;
            case 10: // Set and clear CC Instructions
                break;
            case 11: // Specials - not actually instructions
                result += " " + instruction.toString(8);
                break;
            default:
                result = "bugger";
                break;
        }
        if (historyRec[5] >= 0) {
            result += " ; " + historyRec[5].toString(8);
            if (historyRec[6] >= 0) result += " " + historyRec[6].toString(8);
        }
        console.log(result);
    }
    log.history = [];
}


// Interrupts are stored in a queue in delay order with the delay expressed as
// a difference. For example if the delays were 0, 1, 0 then the first entry
// is active and both the second and third are waiting for one more instruction
// cycle to become active.
// If the current runState is WAIT then skip any delay and go to RUN.

function interrupt(cleanFlag, delay, priority, vector, callback, callarg) {
    var i = CPU.interruptQueue.length;
    if (typeof callback == "undefined") {
        callback = null;
    }
    if (cleanFlag) {
        while (i-- > 0) {
            if (CPU.interruptQueue[i].vector === vector) {
                if (i > 0) {
                    CPU.interruptQueue[i - 1].delay += CPU.interruptQueue[i].delay;
                }
                CPU.interruptQueue.splice(i, 1);
                break;
            }
        }
    }
    if (delay >= 0) { // delay below 0 is just used to remove vector from queue
        if (CPU.runState == STATE.WAIT) { // if currently in wait then resume
            delay = 0;
            CPU.runState = STATE.RUN;
            if (!panel.halt) setTimeout(emulate, 0);
        }
        i = CPU.interruptQueue.length; // queue in delay 'difference' order
        while (i-- > 0) {
            if (CPU.interruptQueue[i].delay > delay) {
                CPU.interruptQueue[i].delay -= delay;
                break;
            }
            delay -= CPU.interruptQueue[i].delay;
        }
        CPU.interruptQueue.splice(i + 1, 0, {
            "delay": delay,
            "priority": priority & 0xe0,
            "vector": vector,
            "callback": callback,
            "callarg": callarg
        });
        if (delay > 0 || (priority & 0xe0) > (CPU.PSW & 0xe0)) {
            CPU.priorityReview = 1; // Schedule an interrupt priority review if required
        }
    }
}


// When a wait instruction is executed do a search through the interrupt list
// to see if we can run something (anything!) which has been delayed. If there is
// something then we don't actually need to enter WAIT state.

function interruptWaitRelease() {
    var savePSW, i;
    savePSW = CPU.PSW & 0xe0;
    i = CPU.interruptQueue.length;
    while (i-- > 0) {
        CPU.interruptQueue[i].delay = 0;
        if (CPU.interruptQueue[i].priority > (CPU.PSW & 0xe0)) {
            CPU.priorityReview = 1;
            return 1; // Found something that can run
        }
    }
    return 0; // No candidates found for WAIT release
}


// When the PSW, PIR or interrupt queue state have changed then it is time to review
// the list of pending interrupts to see if we can invoke one. If nothing changes
// then more reviews are not required until something does.
// Review controlled by the flag CPU.priorityReview

function interruptReview() {
    var highPriority, high, i;
    CPU.priorityReview = 0;
    high = -1;
    highPriority = CPU.PIR & 0xe0;
    if ((i = CPU.interruptQueue.length) > 0) {
        while (i-- > 0) {
            if (CPU.interruptQueue[i].delay > 0) {
                CPU.interruptQueue[i].delay--;
                CPU.priorityReview = 1;
                break; // Decrement only one delay 'difference' per cycle
            }
            if (CPU.interruptQueue[i].callback) {
                if (!CPU.interruptQueue[i].callback(CPU.interruptQueue[i].callarg)) {
                    CPU.interruptQueue.splice(i, 1);
                    high--;
                    continue;
                }
                CPU.interruptQueue[i].callback = null;
            }
            if (CPU.interruptQueue[i].priority > highPriority) {
                highPriority = CPU.interruptQueue[i].priority & 0xe0;
                high = i;
            }
        }
    }
    if (highPriority > (CPU.PSW & 0xe0)) { // check if we found an interrupt
        if (high < 0) {
            trap(0240, 42); // PIR trap
        } else {
            trap(CPU.interruptQueue[high].vector, 44); // BR trap
            CPU.interruptQueue.splice(high, 1);
        }
    }
}


// writePSW() is used to update the CPU Processor Status Word. The PSW should generally
// be written through this routine so that changes can be tracked properly, for example
// the correct register set, the current memory management mode, etc. An exception is
// SPL which writes the priority directly. Note that that N, Z, V, and C flags are
// actually stored separately to the PSW (CPU.PSW) for performance reasons.
//
// CPU.PSW    15 14 13 12 11 10  9  8  7  6  5  4  3  2  1  0
//              CM |  PM |RS|        |PRIORITY| T| N| Z| V| C
// mode 0 kernel 1 super 2 illegal 3 user

function writePSW(newPSW) {
    var i, temp;
    CPU.flagN = newPSW << 12;
    CPU.flagZ = (~newPSW) & 4;
    CPU.flagV = newPSW << 14;
    CPU.flagC = newPSW << 16;
    if ((newPSW ^ CPU.PSW) & 0x800) {
        for (i = 0; i < 6; i++) {
            temp = CPU.registerVal[i];
            CPU.registerVal[i] = CPU.registerAlt[i];
            CPU.registerAlt[i] = temp; // swap to register sets
        }
    }
    CPU.mmuMode = (newPSW >> 14) & 3;
    temp = (CPU.PSW >> 14) & 3;
    if (CPU.mmuMode != temp) {
        CPU.stackPointer[temp] = CPU.registerVal[6];
        CPU.registerVal[6] = CPU.stackPointer[CPU.mmuMode]; // swap to new mode SP
    }
    if ((newPSW & 0xe0) < (CPU.PSW & 0xe0)) {
        CPU.priorityReview = 1; // trigger check of priority levels
    }
    CPU.PSW = newPSW;
}


// readPSW() reassembles the  N, Z, V, and C flags back into the PSW (CPU.PSW)

function readPSW() {
    CPU.PSW = (CPU.PSW & 0xf8f0) | ((CPU.flagN >> 12) & 8) | ((CPU.flagV >> 14) & 2) | ((CPU.flagC >> 16) & 1);
    if (!(CPU.flagZ & 0xffff)) CPU.PSW |= 4;
    return CPU.PSW;
}


// trap() handles all the trap/abort functions. It reads the trap vector from kernel
// D space, changes mode to reflect the new PSW and PC, and then pushes the old PSW and
// PC onto the new mode stack. trap() returns a -1 which is passed up through function
// calls to indicate that a trap/abort has occurred (suspend instruction processing)
// CPU.trapPSW records the first PSW for double trap handling. The special value of -2
// allows console mode to propagate an abort without trapping to the new vector.

function trap(vector, reason) {
    var newPC, newPSW, doubleTrap = 0;
    if (CPU.trapPSW > -2) {
        if (CPU.trapPSW < 0) {
            CPU.trapMask = 0; // No other traps unless we cause one here
            CPU.trapPSW = readPSW(); // Remember original PSW
        } else {
            if (!CPU.mmuMode) {
                vector = 4;
                doubleTrap = 1;
            }
        }
        //LOG_INSTRUCTION(vector, 11, "-trap-");
        if (!(CPU.MMR0 & 0xe000)) {
            CPU.MMR1 = 0xf6f6;
            CPU.MMR2 = vector;
        }
        CPU.mmuMode = 0; // read from kernel D space
        if ((newPC = readWordByVirtual(vector | 0x10000)) >= 0) {
            if ((newPSW = readWordByVirtual(((vector + 2) & 0xffff) | 0x10000)) >= 0) {
                writePSW((newPSW & 0xcfff) | ((CPU.trapPSW >> 2) & 0x3000)); // set new CPU.PSW with previous mode
                if (doubleTrap) {
                    CPU.CPU_Error |= 4;
                    CPU.registerVal[6] = 4;
                }
                if (pushWord(CPU.trapPSW, doubleTrap) >= 0 && pushWord(CPU.registerVal[7], doubleTrap) >= 0) {
                    CPU.registerVal[7] = newPC;
                }
            }
        }
        CPU.trapPSW = -1; // reset flag that we have a trap within a trap
    }
    return -1; // signal that a trap has occurred
}


// mapVirtualToPhysical() does memory management. It converts a 17 bit I/D
// virtual address to a 22 bit physical address (Note: the eight pseudo addresses
// for handling registers are NOT known at this level - those exist only for
// higher level functions). A real PDP 11/70 memory management unit can be enabled separately
// for read and write for diagnostic purposes. This is handled here by having by having
// an enable mask (CPU.mmuEnable) which is tested against the operation access mask
// (accessMask). If there is no match then the virtual address is simply mapped
// as a 16 bit physical address with the upper page going to the IO address space.
// Significant access mask values used are READ_MODE and WRITE_MODE
//
// As an aside it turns out that it is the memory management unit that does odd address
// and non-existant memory trapping: who knew? :-) I thought these would have been
// handled at access time.
//
// When doing mapping CPU.mmuMode is used to decide what address space is to be
// used. 0 = kernel, 1 = supervisor, 2 = illegal, 3 = user. Normally CPU.mmuMode is
// set by the writePSW() function but there are execptions for instructions which
// move data between address spaces (MFPD, MFPI, MTPD, and MTPI) and trap(). These will
// modify CPU.mmuMode outside of writePSW() and then restore it again if all worked. If
// however something happens to cause a trap then no restore is done as writePSW()
// will have been invoked as part of the trap, which will resynchronise CPU.mmuMode
//
// A PDP 11/70 is different to other PDP 11's in that the highest 18 bit space (017000000
// & above) map directly to unibus space - including low memory. This doesn't appear to
// be particularly useful as it restricts maximum system memory - although it does 
// allow software testing of the unibus map. This feature also appears to confuse some
// OSes which test consequetive memory locations to find maximum memory - and on a full
// memory system find themselves accessing low memory again at high addresses.
//
// 15 | 14 | 13 | 12 | 11 | 10 | 9 | 8 | 7 | 6 | 5 | 4 | 3 | 2 | 1 | 0 MMR0
//nonr leng read trap unus unus ena mnt cmp  -mode- i/d  --page--   enable
//
// Map a 17 bit I/D virtual address to a 22 bit physical address

function mapVirtualToPhysical(virtualAddress, accessMask) {
    "use strict";
    var page, pdr, physicalAddress, errorMask = 0;
    //var CPU = window.CPU;
    //if (virtualAddress & ~0x1ffff) panic(89); // check VA range
    //if (!accessMask) panic(93); // Must have READ_MODE or WRITE_MODE
    if (!(accessMask & CPU.mmuEnable)) { // This access does not require the MMU
        physicalAddress = virtualAddress & 0xffff; // virtual address without MMU is 16 bit (no I&D)
        CPU.displayAddress = physicalAddress;
        if (physicalAddress >= IOBASE_VIRT) {
            physicalAddress |= IOBASE_22BIT;
        } else { // no max_memory check in 16 bit mode
            if ((physicalAddress & 1) && !(accessMask & BYTE_MODE)) {
                CPU.CPU_Error |= 0x40;
                return trap(4, 22);
            }
        }
        return physicalAddress;
    } else { // This access must be mapped by the MMU
        CPU.displayAddress = virtualAddress & 0xffff;
        page = virtualAddress >> 13;
        if (!(CPU.MMR3 & CPU.MMR3Map[CPU.mmuMode])) page &= 7;
        pdr = CPU.mmuPDR[CPU.mmuMode][page];
        physicalAddress = ((CPU.mmuPAR[CPU.mmuMode][page] << 6) + (virtualAddress & 0x1fff)) & 0x3fffff;
        if (!(CPU.MMR3 & 0x10)) { // 18 bit mapping needs extra trimming
            physicalAddress &= 0x3ffff;
            if (physicalAddress >= IOBASE_18BIT) physicalAddress |= IOBASE_22BIT;
        }
        if (physicalAddress < MAX_MEMORY) { // Ordinary memory space only needs an odd address check
            if ((physicalAddress & 1) && !(accessMask & BYTE_MODE)) {
                CPU.CPU_Error |= 0x40;
                return trap(4, 26);
            }
        } else { // Higher addresses may require unibus mapping and a check if non-existant
            if (physicalAddress < IOBASE_22BIT) {
                if (physicalAddress >= IOBASE_UNIBUS) {
                    physicalAddress = mapUnibus(physicalAddress & 0x3ffff); // 18bit unibus space
                    if (physicalAddress >= MAX_MEMORY && physicalAddress < IOBASE_22BIT) {
                        CPU.CPU_Error |= 0x10; // Unibus timeout
                        return trap(4, 24); // KB11-EM does this after ABORT handling - KB11-CM before
                    }
                } else {
                    CPU.CPU_Error |= 0x20; // Non-existant main memory
                    return trap(4, 24); //
                }
            }
        }
        switch (pdr & 0x7) { // Check the Access Control Field (ACF) - really a page type
            case 1: // read-only with trap
                errorMask = 0x1000; // MMU trap - then fall thru
            case 2: // read-only
                pdr |= 0x80; // Set A bit
                if (accessMask & WRITE_MODE) {
                    errorMask = 0x2000; // read-only abort
                }
                break;
            case 4: // read-write with read-write trap
                errorMask = 0x1000; // MMU trap - then fall thru
            case 5: // read-write with write trap
                if (accessMask & WRITE_MODE) {
                    errorMask = 0x1000; // MMU trap - then fall thru
                }
            case 6: // read-write
                pdr |= ((accessMask & WRITE_MODE) ? 0xc0 : 0x80); // Set A & W bits
                break;
            default:
                errorMask = 0x8000; // non-resident abort
                break;
        }
        if ((pdr & 0x7f08) !== 0x7f00) { // Skip page length check for most common case (hopefully)
            if (pdr & 0x8) { // Page expands downwards
                if (pdr & 0x7f00) {
                    if ((virtualAddress & 0x1fc0) < ((pdr >> 2) & 0x1fc0)) {
                        errorMask |= 0x4000; // page length error abort
                    }
                }
            } else { // Page expand upwards
                if ((virtualAddress & 0x1fc0) > ((pdr >> 2) & 0x1fc0)) {
                    errorMask |= 0x4000; // page length error abort
                }
            }
        }
        // aborts and traps: log FIRST trap and MOST RECENT abort

        CPU.mmuPDR[CPU.mmuMode][page] = pdr;
        if ((physicalAddress !== 0x3fff7a) || CPU.mmuMode) { // MMR0 is 017777572
            CPU.mmuLastMode = CPU.mmuMode;
            CPU.mmuLastPage = page;
        }
        if (errorMask) {
            if (errorMask & 0xe000) {
                if (CPU.trapPSW >= 0) errorMask |= 0x80; // Instruction complete
                if (!(CPU.MMR0 & 0xe000)) {
                    CPU.MMR0 |= errorMask | (CPU.mmuLastMode << 5) | (CPU.mmuLastPage << 1);
                }
                return trap(0xa8, 28); // 0250
            }
            if (!(CPU.MMR0 & 0xf000)) {
                //if (physicalAddress < 017772200 || physicalAddress > 017777677) {
                if (physicalAddress < 0x3ff480 || physicalAddress > 0x3fffbf) {
                    CPU.MMR0 |= 0x1000; // MMU trap flag
                    if (CPU.MMR0 & 0x0200) {
                        CPU.trapMask |= 2; // MMU trap
                    }
                }
            }
        }
        CPU.displayPhysical = physicalAddress;
        return physicalAddress;
    }
}


function readWordByAddr(physicalAddress) {
    "use strict";
    if (physicalAddress >= MAX_ADDRESS) {
        return CPU.registerVal[physicalAddress - MAX_ADDRESS];
    } else {
        if (physicalAddress >= IOBASE_UNIBUS) {
            return access_iopage(physicalAddress, -1, 0);
        } else {
            if (physicalAddress >= 0) {
                return CPU.memory[physicalAddress >> 1];
            }
        }
    }
    return physicalAddress;
}


function writeWordByAddr(physicalAddress, data) {
    "use strict";
    data &= 0xffff;
    if (physicalAddress >= MAX_ADDRESS) {
        return (CPU.registerVal[physicalAddress - MAX_ADDRESS] = data);
    } else {
        if (physicalAddress >= IOBASE_UNIBUS) {
            return access_iopage(physicalAddress, data, 0);
        } else {
            if (physicalAddress >= 0) {
                return (CPU.memory[physicalAddress >> 1] = data);
            }
        }
    }
    return physicalAddress;
}


function readByteByAddr(physicalAddress) {
    "use strict";
    var result;
    if (physicalAddress >= MAX_ADDRESS) {
        return (CPU.registerVal[physicalAddress - MAX_ADDRESS] & 0xff);
    } else {
        if (physicalAddress >= IOBASE_UNIBUS) {
            return access_iopage(physicalAddress, -1, 1);
        } else {
            if (physicalAddress >= 0) {
                result = CPU.memory[physicalAddress >> 1];
                if (physicalAddress & 1) {
                    result = result >> 8;
                }
                return (result & 0xff);
            }
        }
    }
    return physicalAddress;
}


function writeByteByAddr(physicalAddress, data) {
    "use strict";
    data &= 0xff;
    if (physicalAddress >= MAX_ADDRESS) {
        return (CPU.registerVal[physicalAddress - MAX_ADDRESS] = (CPU.registerVal[physicalAddress - MAX_ADDRESS] & 0xff00) | data);
    } else {
        if (physicalAddress >= IOBASE_UNIBUS) {
            return access_iopage(physicalAddress, data, 1);
        } else {
            if (physicalAddress >= 0) {
                if (physicalAddress & 1) {
                    return (CPU.memory[physicalAddress >> 1] = (data << 8) | (CPU.memory[physicalAddress >> 1] & 0xff));
                } else {
                    return (CPU.memory[physicalAddress >> 1] = (CPU.memory[physicalAddress >> 1] & 0xff00) | data);
                }
            }
        }
    }
    return physicalAddress;
}


function readWordByVirtual(virtualAddress) { // input address is 17 bit (I&D)
    return readWordByAddr(mapVirtualToPhysical(virtualAddress, READ_MODE));
}


function popWord() {
    "use strict";
    var result;
    if ((result = readWordByVirtual(CPU.registerVal[6] | 0x10000)) >= 0) {
        CPU.registerVal[6] = (CPU.registerVal[6] + 2) & 0xffff;
    }
    return result;
}

// Stack limit checks only occur for Kernel mode and are either a yellow warning trap
// after instruction completion, or a red abort which stops the current instruction.

function stackCheck(virtualAddress) {
    "use strict";
    if (!CPU.mmuMode) { // Kernel mode 0 checking only
        if (virtualAddress <= CPU.stackLimit || virtualAddress >= 0xfffe) {
            if (virtualAddress + 32 <= CPU.stackLimit || virtualAddress >= 0xfffe) {
                CPU.CPU_Error |= 4; // Red stack
                CPU.registerVal[6] = 4;
                return trap(4, 38);
            }
            CPU.CPU_Error |= 8; // Yellow
            CPU.trapMask |= 4;
        }
    }
    return virtualAddress;
}


function pushWord(data, skipLimitCheck) {
    "use strict";
    var physicalAddress, virtualAddress;
    CPU.registerVal[6] = virtualAddress = (CPU.registerVal[6] - 2) & 0xffff; // BSD meeds SP updated before any fault :-(
    if (!(CPU.MMR0 & 0xe000)) {
        CPU.MMR1 = (CPU.MMR1 << 8) | 0xf6;
    }
    if (!skipLimitCheck) {
        if ((virtualAddress = stackCheck(virtualAddress)) < 0) {
            return virtualAddress;
        }
    }
    if ((physicalAddress = mapVirtualToPhysical(virtualAddress | 0x10000, WRITE_MODE)) >= 0) {
        return writeWordByAddr(physicalAddress, data);
    }
    return physicalAddress;
}


// getVirtualByMode() maps a six bit operand to a 17 bit I/D virtual address space.
// Instruction operands are six bits in length - three bits for the mode and three
// for the register. The 17th I/D bit in the resulting virtual address represents
// whether the reference is to Instruction space or Data space - which depends on
// combination of the mode and whether the register is the Program Counter (register 7).
//
// The eight modes are:-
//		0	R			no valid virtual address
//		1	(R)			operand from I/D depending if R = 7
//		2	(R)+		operand from I/D depending if R = 7
//		3	@(R)+		address from I/D depending if R = 7 and operand from D space
//		4	-(R)		operand from I/D depending if R = 7
//		5	@-(R)		address from I/D depending if R = 7 and operand from D space
//		6	x(R)		x from I space but operand from D space
//		7	@x(R)		x from I space but address and operand from D space
//
// Stack limit checks are implemented for modes 1, 2, 4 & 6 (!)
//
// Also need to keep CPU.MMR1 updated as this stores which registers have been
// incremented and decremented so that the OS can reset and restart an instruction
// if a page fault occurs.
//
// Convert a six bit instruction operand to a 17 bit I/D virtual address

function getVirtualByMode(addressMode, accessMode) {
    "use strict";
    var virtualAddress, stepSize, reg = addressMode & 7;
    switch ((addressMode >> 3) & 7) {
        case 0: // Mode 0: Registers don't have a virtual address so trap!
            return trap(4, 34); // trap for invalid virtual address
        case 1: // Mode 1: (R)
            if (reg == 6 && (accessMode & WRITE_MODE)) {
                if ((virtualAddress = stackCheck(CPU.registerVal[6])) < 0) {
                    return virtualAddress;
                }
            }
            return (reg === 7 ? CPU.registerVal[reg] : (CPU.registerVal[reg] | 0x10000));
        case 2: // Mode 2: (R)+
            stepSize = 2;
            virtualAddress = CPU.registerVal[reg];
            if (reg == 6 && (accessMode & WRITE_MODE)) {
                if ((virtualAddress = stackCheck(virtualAddress)) < 0) {
                    return virtualAddress;
                }
            }
            if (reg !== 7) {
                virtualAddress |= 0x10000;
                if (reg < 6 && (accessMode & BYTE_MODE)) {
                    stepSize = 1;
                }
            }
            break;
        case 3: // Mode 3: @(R)+
            stepSize = 2;
            virtualAddress = CPU.registerVal[reg];
            if (reg !== 7) virtualAddress |= 0x10000;
            if ((virtualAddress = readWordByVirtual(virtualAddress)) < 0) {
                return virtualAddress;
            }
            if (reg === 7) {
                //LOG_ADDRESS(virtualAddress); // @#n not operational
            }
            virtualAddress |= 0x10000;
            break;
        case 4: // Mode 4: -(R)
            stepSize = -2;
            if (reg < 6 && (accessMode & BYTE_MODE)) stepSize = -1;
            virtualAddress = (CPU.registerVal[reg] + stepSize) & 0xffff;
            if (reg == 6 && (accessMode & WRITE_MODE)) {
                if ((virtualAddress = stackCheck(virtualAddress)) < 0) {
                    return virtualAddress;
                }
            }
            if (reg !== 7) {
                virtualAddress |= 0x10000;
            }
            break;
        case 5: // Mode 5: @-(R)
            stepSize = -2;
            virtualAddress = (CPU.registerVal[reg] - 2) & 0xffff;
            if (reg !== 7) virtualAddress |= 0x10000;
            if ((virtualAddress = readWordByVirtual(virtualAddress)) < 0) {
                return virtualAddress;
            }
            virtualAddress |= 0x10000;
            break;
        case 6: // Mode 6: d(R)
            if ((virtualAddress = readWordByVirtual(CPU.registerVal[7])) < 0) {
                return virtualAddress;
            }
            CPU.registerVal[7] = (CPU.registerVal[7] + 2) & 0xffff;
            if (reg < 7) {
                //LOG_ADDRESS(virtualAddress);
                virtualAddress = (virtualAddress + CPU.registerVal[reg]) & 0xffff;
            } else {
                virtualAddress = (virtualAddress + CPU.registerVal[reg]) & 0xffff;
                //LOG_ADDRESS(virtualAddress);
            }
            if (reg == 6 && (accessMode & WRITE_MODE)) {
                if ((virtualAddress = stackCheck(virtualAddress)) < 0) {
                    return virtualAddress;
                }
            }
            return virtualAddress | 0x10000;
        case 7: // Mode 7: @d(R)
            if ((virtualAddress = readWordByVirtual(CPU.registerVal[7])) < 0) {
                return virtualAddress;
            }
            CPU.registerVal[7] = (CPU.registerVal[7] + 2) & 0xffff;
            if (reg < 7) {
                //LOG_ADDRESS(virtualAddress);
                virtualAddress = (virtualAddress + CPU.registerVal[reg]) & 0xffff;
            } else {
                virtualAddress = (virtualAddress + CPU.registerVal[reg]) & 0xffff;
                //LOG_ADDRESS(virtualAddress);
            }
            if ((virtualAddress = readWordByVirtual(virtualAddress | 0x10000)) < 0) {
                return virtualAddress;
            }
            return virtualAddress | 0x10000; // @x
    }
    CPU.registerVal[reg] = (CPU.registerVal[reg] + stepSize) & 0xffff;
    if (!(CPU.MMR0 & 0xe000)) {
        CPU.MMR1 = (CPU.MMR1 << 8) | ((stepSize << 3) & 0xf8) | reg;
    }
    return virtualAddress;
}


function getAddrByMode(addressMode, accessMode) {
    "use strict";
    var result;
    if (!(addressMode & 0x38)) {
        return MAX_ADDRESS + (addressMode & 7); // Registers have special addresses above maximum address
    } else {
        if ((result = getVirtualByMode(addressMode, accessMode)) >= 0) {
            result = mapVirtualToPhysical(result, accessMode);
        }
        return result;
    }
}


function readWordByMode(addressMode) {
    "use strict";
    var result;
    if (!(addressMode & 0x38)) {
        result = CPU.registerVal[addressMode & 7];
        //LOG_SOURCE(result);
    } else {
        if ((result = getAddrByMode(addressMode, READ_MODE)) >= 0) {
            if ((result = readWordByAddr(result)) >= 0) {
                //LOG_SOURCE(result);
            }
        }
    }
    return result;
}


function readByteByMode(addressMode) {
    "use strict";
    var result;
    if (!(addressMode & 0x38)) {
        result = CPU.registerVal[addressMode & 7] & 0xff;
        //LOG_SOURCE(result);
    } else {
        if ((result = getAddrByMode(addressMode, READ_MODE | BYTE_MODE)) >= 0) {
            if ((result = readByteByAddr(result)) >= 0) {
                //LOG_SOURCE(result);
            }
        }
    }
    return result;
}


// branch() calculates the branch to PC from a branch instruction offset

function branch(PC, instruction) {
    return (PC + ((instruction & 0200 ? instruction | 0xff00 : instruction & 0xff) << 1)) & 0xffff;
}


// Most instruction read operations use a 6 bit instruction operand via
//   readWordByMode(instruction operand). If the result is negative then
// something failed and generally a trap or abort has already been invoked.
// The code for this would generally look like:
//   if ((src = readWordByMode(instruction >> 6)) >= 0) {
//         success - do operation
//
// For each Word function there are usually corresponding Byte functions, eg readByteByMode()
//
// Write operations are just a special case of read/write and there are
// no special functions to support them, mainly because they would only be used by MOV
// and CLR instructions. Maybe they should be added for consistency?
//
// Read/Write operations are harder than read because we want to do memory mapping
// only once. So the strategy is to get a physical address, use it to read
// the operand, and if nothing went wrong use it to do a write.
// The instruction code for this would generally look like:
//   if ((dst = readWordByAddr(dstAddr = getAddrByMode(instruction, MODIFY_WORD))) >= 0) {
//       result = something and dst
//       if (writeWordByAddr(dstAddr, result) >= 0) {
//
// Note that the getAddrByMode() function requires that we specify a mode mask
// to tell it if we are doing a read or write (or modify for both), or accessing
// a byte. This is required by memory management in advance of actually doing an operation.
//
// For performance reasons many instructions have a special case optimization for register
// read/write (operand mode 0). Although the code would work without this (because
// getAddrByMode() will return a special pseudo physical address for registers),
// it is much quicker to bypass address generation and directly access the register.
// The code for this would generally look like:
//   if (!(operand & 0x38)) {
//      CPU.registerVal[operand & 7] = something
//   } else {
//      normal non-register code
//
// Some instructions (eg JMP, JSR, MTPx..) require the address of an operand.
// The code for this would generally look like:
//   if ((virtualAddress = getVirtualByMode(instruction, 0)) >= 0) {
//
// Note that the access mode used here is 0: no read, no write, no byte - just get the address.
//
// Most of these functions work in layers. For example:
//  readWordByMode() fetches an operand after converting an instruction operand to
//    a physical address, which is generated using getAddrByMode()
//    The physical addresses is then accessed using readWordByAddr()
//  getAddrByMode() first converts an instruction operand to a 17 bit I/D virtual address
//    using getVirtualByMode() - which it then converts to a physical
//    address using mapVirtualToPhysical() to represent a standard 22 bit PDP 11/70 memory
//    and unibus address, or to one of eight special pseudo addresses for registers.
//  readWordByAddr() / writeWordByAddr() either access registers directly, memory as
//    CPU.memory[], or access the unibus I/O space through access_iopage()
//
// access_iopage() does a unibus read if it is passed -1 as data, or a write otherwise.
//
// CPU flags are stored outside of the PSW for performance reasons. A call to
// readPSW() will assemble them back into the PSW. Writes to the PSW should generally
// be through writePSW() as it needs to track which register set is in use, the memory
// management mode, whether priority has changed etc.
// Individual flags are CPU.flagC, CPU.flagN, CPU.flagV, and CPU.flagZ. These hold
// the value of the last result affecting them. So for example bit 16 of CPU.flagC
// is the only useful bit it contains. Likewise bit 15 of CPU.flagN and CPU.flagV is
// the only bit they use. For CPU.flagZ the lower 16 bits must be tested to see if
// the result was zero or not.
//
// All traps and aborts go through the trap() function. It returns a -1 value which
// is then passed up through other function layers and interpreted as an indicator
// that something has gone wrong, and that no futher processing is to be done for the
// current instruction.
//
// Instruction execution is performed by the step() function which processes a batch of
// instructions. The current strategy is to execute 5000 instructions repeating until
// 20 milliseconds (50 hz) have passed. 
// Batching instructions in this way is required in Javascript as it is necessary
// to relinquish control periodically to let timer and I/O functions execute, and to
// update the console lights.


function step(loopCount) {
    var instruction,
        src,
        dst,
        dstAddr,
        result,
        virtualAddress, savePSW, reg;
    var loopTime = Date.now() + 20; // execute for 20 ms (50 Hz);
    var CPU = window.CPU;
    do {
        // If something has changed review priority - with one instruction delay after SPL
        if (CPU.priorityReview) {
            if (!(--CPU.priorityReview)) {
                interruptReview();
            }
        }
        if (CPU.trapMask) { // check for any post instruction traps pending
            if (CPU.trapMask & 2) {
                trap(0250, 52); // MMU trap has priority
            } else {
                if (CPU.trapMask & 4) {
                    trap(4, 54); // then SP trap
                } else {
                    if (CPU.trapMask & 0x10) trap(014, 56); // and finally a T-bit trap
                }
            }
        }
        if (!(CPU.MMR0 & 0xe000)) {
            CPU.MMR1 = 0;
            CPU.MMR2 = CPU.registerVal[7];
        }
        // Remember if T-bit trap required at the end of this instruction
        CPU.trapMask = CPU.PSW & 0x10;
        if ((instruction = readWordByVirtual(CPU.registerVal[7])) >= 0) {
            //if (CPU.registerVal[7] >= 0157220) { // DDEEBBUUGG
            //    console.log("PC " + CPU.registerVal[7].toString(8) + " instruction: " + instruction.toString(8) + " R0: " + CPU.registerVal[0].toString(8) + " R4: " + CPU.registerVal[4].toString(8));
            //}
            CPU.registerVal[7] = (CPU.registerVal[7] + 2) & 0xffff;
            switch (instruction & 0170000) { // Double operand instructions xxSSDD
                case 0010000: // MOV  01SSDD
                    //LOG_INSTRUCTION(instruction, 2, "MOV");
                    if ((src = readWordByMode(instruction >> 6)) >= 0) {
                        if (!(instruction & 0x38)) {
                            CPU.registerVal[instruction & 7] = src;
                            CPU.flagN = CPU.flagZ = src;
                            CPU.flagV = 0;
                        } else {
                            if ((dstAddr = getAddrByMode(instruction, WRITE_MODE)) >= 0) {
                                if (writeWordByAddr(dstAddr, src) >= 0) {
                                    CPU.flagN = CPU.flagZ = src;
                                    CPU.flagV = 0;
                                }
                            }
                        }
                    }
                    break;
                case 0020000: // CMP 02SSDD
                    //LOG_INSTRUCTION(instruction, 2, "CMP");
                    if ((src = readWordByMode(instruction >> 6)) >= 0) {
                        if ((dst = readWordByMode(instruction)) >= 0) {
                            result = src - dst;
                            CPU.flagN = CPU.flagZ = CPU.flagC = result;
                            CPU.flagV = (src ^ dst) & (src ^ result);
                        }
                    }
                    break;
                case 0030000: // BIT 03SSDD
                    //LOG_INSTRUCTION(instruction, 2, "BIT");
                    if ((src = readWordByMode(instruction >> 6)) >= 0) {
                        if ((dst = readWordByMode(instruction)) >= 0) {
                            CPU.flagN = CPU.flagZ = src & dst;
                            CPU.flagV = 0;
                        }
                    }
                    break;
                case 0040000: // BIC 04SSDD
                    //LOG_INSTRUCTION(instruction, 2, "BIC");
                    if ((src = readWordByMode(instruction >> 6)) >= 0) {
                        if (!(instruction & 0x38)) {
                            result = CPU.registerVal[instruction & 7] &= ~src;
                            CPU.flagN = CPU.flagZ = result;
                            CPU.flagV = 0;
                        } else {
                            if ((dst = readWordByAddr(dstAddr = getAddrByMode(instruction, MODIFY_WORD))) >= 0) {
                                result = dst & ~src;
                                if (writeWordByAddr(dstAddr, result) >= 0) {
                                    CPU.flagN = CPU.flagZ = result;
                                    CPU.flagV = 0;
                                }
                            }
                        }
                    }
                    break;
                case 0050000: // BIS 05SSDD
                    //LOG_INSTRUCTION(instruction, 2, "BIS");
                    if ((src = readWordByMode(instruction >> 6)) >= 0) {
                        if (!(instruction & 0x38)) {
                            result = CPU.registerVal[instruction & 7] |= src;
                            CPU.flagN = CPU.flagZ = result;
                            CPU.flagV = 0;
                        } else {
                            if ((dst = readWordByAddr(dstAddr = getAddrByMode(instruction, MODIFY_WORD))) >= 0) {
                                result = dst | src;
                                if (writeWordByAddr(dstAddr, result) >= 0) {
                                    CPU.flagN = CPU.flagZ = result;
                                    CPU.flagV = 0;
                                }
                            }
                        }
                    }
                    break;
                case 0060000: // ADD 06SSDD
                    //LOG_INSTRUCTION(instruction, 2, "ADD");
                    if ((src = readWordByMode(instruction >> 6)) >= 0) {
                        if (!(instruction & 0x38)) {
                            reg = instruction & 7;
                            dst = CPU.registerVal[reg];
                            CPU.registerVal[reg] = (result = src + dst) & 0xffff;
                            CPU.flagN = CPU.flagZ = CPU.flagC = result;
                            CPU.flagV = (src ^ result) & (dst ^ result);
                        } else {
                            if ((dst = readWordByAddr(dstAddr = getAddrByMode(instruction, MODIFY_WORD))) >= 0) {
                                result = src + dst;
                                if (writeWordByAddr(dstAddr, result) >= 0) {
                                    CPU.flagN = CPU.flagZ = CPU.flagC = result;
                                    CPU.flagV = (src ^ result) & (dst ^ result);
                                }
                            }
                        }
                    }
                    break;
                case 0110000: // MOVB 11SSDD
                    //LOG_INSTRUCTION(instruction, 2, "MOVB");
                    if ((src = readByteByMode(instruction >> 6)) >= 0) {
                        if (!(instruction & 0x38)) {
                            if (src & 0200) src |= 0xff00; // movb sign extends register to word size
                            CPU.registerVal[instruction & 7] = src;
                            CPU.flagN = CPU.flagZ = src;
                            CPU.flagV = 0;
                        } else {
                            if ((dstAddr = getAddrByMode(instruction, WRITE_MODE | BYTE_MODE)) >= 0) { // write byte
                                if (writeByteByAddr(dstAddr, src) >= 0) {
                                    CPU.flagN = CPU.flagZ = src << 8;
                                    CPU.flagV = 0;
                                }
                            }
                        }
                    }
                    break;
                case 0120000: // CMPB 12SSDD
                    //LOG_INSTRUCTION(instruction, 2, "CMPB");
                    if ((src = readByteByMode(instruction >> 6)) >= 0) {
                        if ((dst = readByteByMode(instruction)) >= 0) {
                            result = src - dst;
                            CPU.flagN = CPU.flagZ = CPU.flagC = result << 8;
                            CPU.flagV = ((src ^ dst) & (src ^ result)) << 8;
                        }
                    }
                    break;
                case 0130000: // BITB 13SSDD
                    //LOG_INSTRUCTION(instruction, 2, "BITB");
                    if ((src = readByteByMode(instruction >> 6)) >= 0) {
                        if ((dst = readByteByMode(instruction)) >= 0) {
                            CPU.flagN = CPU.flagZ = (src & dst) << 8;
                            CPU.flagV = 0;
                        }
                    }
                    break;
                case 0140000: // BICB 14SSDD
                    //LOG_INSTRUCTION(instruction, 2, "BICB");
                    if ((src = readByteByMode(instruction >> 6)) >= 0) {
                        if ((dst = readByteByAddr(dstAddr = getAddrByMode(instruction, MODIFY_BYTE))) >= 0) {
                            result = dst & ~src;
                            if (writeByteByAddr(dstAddr, result) >= 0) {
                                CPU.flagN = CPU.flagZ = result << 8;
                                CPU.flagV = 0;
                            }
                        }
                    }
                    break;
                case 0150000: // BISB 15SSDD
                    //LOG_INSTRUCTION(instruction, 2, "BISB");
                    if ((src = readByteByMode(instruction >> 6)) >= 0) {
                        if ((dst = readByteByAddr(dstAddr = getAddrByMode(instruction, MODIFY_BYTE))) >= 0) {
                            result = dst | src;
                            if (writeByteByAddr(dstAddr, result) >= 0) {
                                CPU.flagN = CPU.flagZ = result << 8;
                                CPU.flagV = 0;
                            }
                        }
                    }
                    break;
                case 0160000: // SUB 16SSDD
                    //LOG_INSTRUCTION(instruction, 2, "SUB");
                    if ((src = readWordByMode(instruction >> 6)) >= 0) {
                        if (!(instruction & 0x38)) {
                            reg = instruction & 7;
                            dst = CPU.registerVal[reg];
                            CPU.registerVal[reg] = (result = dst - src) & 0xffff;
                            CPU.flagN = CPU.flagZ = CPU.flagC = result;
                            CPU.flagV = (src ^ dst) & (dst ^ result);
                        } else {
                            if ((dst = readWordByAddr(dstAddr = getAddrByMode(instruction, MODIFY_WORD))) >= 0) {
                                result = dst - src;
                                if (writeWordByAddr(dstAddr, result) >= 0) {
                                    CPU.flagN = CPU.flagZ = CPU.flagC = result;
                                    CPU.flagV = (src ^ dst) & (dst ^ result);
                                }
                            }
                        }
                    }
                    break;
                default:
                    switch (instruction & 0177000) { // Misc instructions xxRDD
                        case 04000: // JSR 004RDD
                            //LOG_INSTRUCTION(instruction, 3, "JSR");
                            if ((virtualAddress = getVirtualByMode(instruction, 0)) >= 0) {
                                reg = (instruction >> 6) & 7;
                                if (pushWord(CPU.registerVal[reg], 0) >= 0) {
                                    CPU.registerVal[reg] = CPU.registerVal[7];
                                    CPU.registerVal[7] = virtualAddress & 0xffff;
                                }
                            }
                            break;
                        case 0070000: // MUL 070RSS
                            //LOG_INSTRUCTION(instruction, 3, "MUL");
                            if ((src = readWordByMode(instruction)) >= 0) {
                                reg = (instruction >> 6) & 7;
                                if (src & 0x8000) src |= ~0xffff;
                                dst = CPU.registerVal[reg];
                                if (dst & 0x8000) dst |= ~0xffff;
                                result = ~~(src * dst);
                                CPU.registerVal[reg] = (result >> 16) & 0xffff;
                                CPU.registerVal[reg | 1] = result & 0xffff;
                                CPU.flagN = result >> 16;
                                CPU.flagZ = CPU.flagN | result;
                                CPU.flagC = CPU.flagV = 0;
                                if (result < -32768 || result > 32767) CPU.flagC = 0x10000;
                            }
                            break;
                        case 0071000: // DIV 071RSS
                            //LOG_INSTRUCTION(instruction, 3, "DIV");
                            if ((src = readWordByMode(instruction)) >= 0) {
                                if (!src) {
                                    CPU.flagN = 0; // NZVC
                                    CPU.flagZ = 0;
                                    CPU.flagV = 0x8000;
                                    CPU.flagC = 0x10000; // divide by zero
                                } else {
                                    reg = (instruction >> 6) & 7;
                                    dst = (CPU.registerVal[reg] << 16) | CPU.registerVal[reg | 1];
                                    CPU.flagC = CPU.flagV = 0;
                                    if (src & 0x8000) src |= ~0xffff;
                                    result = ~~(dst / src);
                                    if (result >= -32768 && result <= 32767) {
                                        CPU.registerVal[reg] = result & 0xffff;
                                        CPU.registerVal[reg | 1] = (dst - (result * src)) & 0xffff;
                                        CPU.flagZ = (result >> 16) | result;
                                        CPU.flagN = result >> 16;
                                    } else {
                                        CPU.flagV = 0x8000; // overflow - following are indeterminate
                                        CPU.flagZ = (result >> 15) | result; // dodgy
                                        CPU.flagN = dst >> 16; // just as dodgy
                                        if (src === -1 && CPU.registerVal[reg] === 0xfffe) CPU.registerVal[reg] = CPU.registerVal[reg | 1] = 1; // etc
                                    }
                                }
                            }
                            break;
                        case 072000: // ASH 072RSS
                            //LOG_INSTRUCTION(instruction, 3, "ASH");
                            if ((src = readWordByMode(instruction)) >= 0) {
                                reg = (instruction >> 6) & 7;
                                result = CPU.registerVal[reg];
                                if (result & 0x8000) result |= 0xffff0000;
                                CPU.flagC = CPU.flagV = 0;
                                src &= 077;
                                if (src & 040) { // shift right
                                    src = 64 - src;
                                    if (src > 16) src = 16;
                                    CPU.flagC = result << (17 - src);
                                    result = result >> src;
                                } else {
                                    if (src) {
                                        if (src > 16) {
                                            CPU.flagV = result;
                                            result = 0;
                                        } else {
                                            result = result << src;
                                            CPU.flagC = result;
                                            dst = (result >> 15) & 0xffff; // check successive sign bits
                                            if (dst && dst !== 0xffff) CPU.flagV = 0x8000;
                                        }
                                    }
                                }
                                CPU.registerVal[reg] = result & 0xffff;
                                CPU.flagN = CPU.flagZ = result;
                            }
                            break;
                        case 073000: // ASHC 073RSS
                            //LOG_INSTRUCTION(instruction, 3, "ASHC");
                            if ((src = readWordByMode(instruction)) >= 0) {
                                reg = (instruction >> 6) & 7;
                                dst = (CPU.registerVal[reg] << 16) | CPU.registerVal[reg | 1];
                                CPU.flagC = CPU.flagV = 0;
                                src &= 077;
                                if (src & 040) {
                                    src = 64 - src;
                                    if (src > 32) src = 32;
                                    result = dst >> (src - 1);
                                    CPU.flagC = result << 16;
                                    result >>= 1;
                                    if (dst & 0x80000000) result |= 0xffffffff << (32 - src);
                                } else {
                                    if (src) { // shift left
                                        result = dst << (src - 1);
                                        CPU.flagC = result >> 15;
                                        result <<= 1;
                                        if (src > 32) src = 32;
                                        dst = dst >> (32 - src);
                                        if (dst) {
                                            dst |= (0xffffffff << src) & 0xffffffff;
                                            if (dst !== 0xffffffff) CPU.flagV = 0x8000;
                                        }
                                    } else {
                                        result = dst;
                                    }
                                }
                                CPU.registerVal[reg] = (result >> 16) & 0xffff;
                                CPU.registerVal[reg | 1] = result & 0xffff;
                                CPU.flagN = result >> 16;
                                CPU.flagZ = result >> 16 | result;
                            }
                            break;
                        case 0074000: // XOR 074RSS
                            //LOG_INSTRUCTION(instruction, 3, "XOR");
                            src = CPU.registerVal[(instruction >> 6) & 7];
                            if (!(instruction & 0x38)) {
                                result = CPU.registerVal[instruction & 7] ^= src;
                                CPU.flagN = CPU.flagZ = result;
                                CPU.flagV = 0;
                            } else {
                                if ((dst = readWordByAddr(dstAddr = getAddrByMode(instruction, MODIFY_WORD))) >= 0) {
                                    result = dst ^ src;
                                    if (writeWordByAddr(dstAddr, result) >= 0) {
                                        CPU.flagN = CPU.flagZ = result;
                                        CPU.flagV = 0;
                                    }
                                }
                            }
                            break;
                        case 0077000: // SOB 077Rnn
                            //LOG_INSTRUCTION(instruction, 5, "SOB");
                            reg = (instruction >> 6) & 7;
                            if ((CPU.registerVal[reg] = ((CPU.registerVal[reg] - 1) & 0xffff))) {
                                CPU.registerVal[7] = (CPU.registerVal[7] - ((instruction & 077) << 1)) & 0xffff;
                            }
                            break;
                        default:
                            switch (instruction & 0177400) { // Program control instructions & traps
                                case 0000400: // BR
                                    //LOG_INSTRUCTION(instruction, 4, "BR");
                                    CPU.registerVal[7] = branch(CPU.registerVal[7], instruction);
                                    break;
                                case 0001000: // BNE
                                    //LOG_INSTRUCTION(instruction, 4, "BNE");
                                    if (CPU.flagZ & 0xffff) CPU.registerVal[7] = branch(CPU.registerVal[7], instruction);
                                    break;
                                case 0001400: // BEQ
                                    //LOG_INSTRUCTION(instruction, 4, "BEQ");
                                    if (!(CPU.flagZ & 0xffff)) CPU.registerVal[7] = branch(CPU.registerVal[7], instruction);
                                    break;
                                case 0002000: // BGE
                                    //LOG_INSTRUCTION(instruction, 4, "BGE");
                                    if ((CPU.flagN & 0x8000) === (CPU.flagV & 0x8000)) CPU.registerVal[7] = branch(CPU.registerVal[7], instruction);
                                    break;
                                case 0002400: // BLT
                                    //LOG_INSTRUCTION(instruction, 4, "BLT");
                                    if ((CPU.flagN & 0x8000) !== (CPU.flagV & 0x8000)) CPU.registerVal[7] = branch(CPU.registerVal[7], instruction);
                                    break;
                                case 0003000: // BGT
                                    //LOG_INSTRUCTION(instruction, 4, "BGT");
                                    if ((CPU.flagZ & 0xffff) && ((CPU.flagN & 0x8000) === (CPU.flagV & 0x8000))) CPU.registerVal[7] = branch(CPU.registerVal[7], instruction);
                                    break;
                                case 0003400: // BLE
                                    //LOG_INSTRUCTION(instruction, 4, "BLE");
                                    if (!(CPU.flagZ & 0xffff) || ((CPU.flagN & 0x8000) !== (CPU.flagV & 0x8000))) CPU.registerVal[7] = branch(CPU.registerVal[7], instruction);
                                    break;
                                case 0100000: // BPL
                                    //LOG_INSTRUCTION(instruction, 4, "BPL");
                                    if (!(CPU.flagN & 0x8000)) CPU.registerVal[7] = branch(CPU.registerVal[7], instruction);
                                    break;
                                case 0101000: // BHI
                                    //LOG_INSTRUCTION(instruction, 4, "BHI");
                                    if (!(CPU.flagC & 0x10000) && (CPU.flagZ & 0xffff)) CPU.registerVal[7] = branch(CPU.registerVal[7], instruction);
                                    break;
                                case 0100400: // BMI
                                    //LOG_INSTRUCTION(instruction, 4, "BMI");
                                    if ((CPU.flagN & 0x8000)) CPU.registerVal[7] = branch(CPU.registerVal[7], instruction);
                                    break;
                                case 0101400: // BLOS
                                    //LOG_INSTRUCTION(instruction, 4, "BLOS");
                                    if ((CPU.flagC & 0x10000) || !(CPU.flagZ & 0xffff)) CPU.registerVal[7] = branch(CPU.registerVal[7], instruction);
                                    break;
                                case 0102000: // BVC
                                    //LOG_INSTRUCTION(instruction, 4, "BVC");
                                    if (!(CPU.flagV & 0x8000)) CPU.registerVal[7] = branch(CPU.registerVal[7], instruction);
                                    break;
                                case 0102400: // BVS
                                    //LOG_INSTRUCTION(instruction, 4, "BVS");
                                    if ((CPU.flagV & 0x8000)) CPU.registerVal[7] = branch(CPU.registerVal[7], instruction);
                                    break;
                                case 0103000: // BCC
                                    //LOG_INSTRUCTION(instruction, 4, "BCC");
                                    if (!(CPU.flagC & 0x10000)) CPU.registerVal[7] = branch(CPU.registerVal[7], instruction);
                                    break;
                                case 0103400: // BCS
                                    //LOG_INSTRUCTION(instruction, 4, "BCS");
                                    if (CPU.flagC & 0x10000) CPU.registerVal[7] = branch(CPU.registerVal[7], instruction);
                                    break;
                                case 0104000: // EMT 104000 -> 104377
                                    //LOG_INSTRUCTION(instruction, 7, "EMT");
                                    trap(030, 2);
                                    break;
                                case 0104400: // TRAP 104400 -> 104777
                                    //LOG_INSTRUCTION(instruction, 7, "TRAP");
                                    trap(034, 4);
                                    break;
                                default:
                                    switch (instruction & 0177700) { // Single operand instructions xxxxDD
                                        case 0000100: // JMP 0001DD
                                            //LOG_INSTRUCTION(instruction, 1, "JMP");
                                            if ((virtualAddress = getVirtualByMode(instruction, 0)) >= 0) {
                                                CPU.registerVal[7] = virtualAddress & 0xffff;
                                            }
                                            break;
                                        case 0000300: // SWAB 0003DD
                                            //LOG_INSTRUCTION(instruction, 1, "SWAB");
                                            if ((dst = readWordByAddr(dstAddr = getAddrByMode(instruction, MODIFY_WORD))) >= 0) {
                                                result = (dst << 8) | (dst >> 8);
                                                if (writeWordByAddr(dstAddr, result) >= 0) {
                                                    CPU.flagN = CPU.flagZ = dst & 0xff00;
                                                    CPU.flagV = CPU.flagC = 0;
                                                }
                                            }
                                            break;
                                        case 0005000: // CLR 0050DD
                                            //LOG_INSTRUCTION(instruction, 1, "CLR");
                                            if (!(instruction & 0x38)) {
                                                CPU.registerVal[instruction & 7] = 0;
                                                CPU.flagN = CPU.flagC = CPU.flagV = CPU.flagZ = 0;
                                            } else {
                                                if ((dstAddr = getAddrByMode(instruction, WRITE_MODE)) >= 0) { // write word
                                                    if (writeWordByAddr(dstAddr, 0) >= 0) {
                                                        CPU.flagN = CPU.flagC = CPU.flagV = CPU.flagZ = 0;
                                                    }
                                                }
                                            }
                                            break;
                                        case 0005100: // COM 0051DD
                                            //LOG_INSTRUCTION(instruction, 1, "COM");
                                            if ((dst = readWordByAddr(dstAddr = getAddrByMode(instruction, MODIFY_WORD))) >= 0) {
                                                result = ~dst;
                                                if (writeWordByAddr(dstAddr, result) >= 0) {
                                                    CPU.flagN = CPU.flagZ = result;
                                                    CPU.flagC = 0x10000;
                                                    CPU.flagV = 0;
                                                }
                                            }
                                            break;
                                        case 0005200: // INC 0052DD
                                            //LOG_INSTRUCTION(instruction, 1, "INC");
                                            if (!(instruction & 0x38)) {
                                                reg = instruction & 7;
                                                dst = CPU.registerVal[reg];
                                                result = dst + 1;
                                                CPU.registerVal[reg] = result & 0xffff;
                                                CPU.flagN = CPU.flagZ = result;
                                                CPU.flagV = result & (result ^ dst);
                                            } else {
                                                if ((dst = readWordByAddr(dstAddr = getAddrByMode(instruction, MODIFY_WORD))) >= 0) {
                                                    result = dst + 1;
                                                    if (writeWordByAddr(dstAddr, result) >= 0) {
                                                        CPU.flagN = CPU.flagZ = result;
                                                        CPU.flagV = result & (result ^ dst);
                                                    }
                                                }
                                            }
                                            break;
                                        case 0005300: // DEC 0053DD
                                            //LOG_INSTRUCTION(instruction, 1, "DEC");
                                            if (!(instruction & 0x38)) {
                                                reg = instruction & 7;
                                                dst = CPU.registerVal[reg];
                                                result = dst - 1;
                                                CPU.registerVal[reg] = result & 0xffff;
                                                CPU.flagN = CPU.flagZ = result;
                                                CPU.flagV = (result ^ dst) & dst;
                                            } else {
                                                if ((dst = readWordByAddr(dstAddr = getAddrByMode(instruction, MODIFY_WORD))) >= 0) {
                                                    result = dst - 1;
                                                    if (writeWordByAddr(dstAddr, result) >= 0) {
                                                        CPU.flagN = CPU.flagZ = result;
                                                        CPU.flagV = (result ^ dst) & dst;
                                                    }
                                                }
                                            }
                                            break;
                                        case 0005400: // NEG 0054DD
                                            //LOG_INSTRUCTION(instruction, 1, "NEG");
                                            if ((dst = readWordByAddr(dstAddr = getAddrByMode(instruction, MODIFY_WORD))) >= 0) {
                                                result = -dst;
                                                if (writeWordByAddr(dstAddr, result) >= 0) {
                                                    CPU.flagC = CPU.flagN = CPU.flagZ = result;
                                                    CPU.flagV = result & dst;
                                                }
                                            }
                                            break;
                                        case 0005500: // ADC 0055DD
                                            //LOG_INSTRUCTION(instruction, 1, "ADC");
                                            if ((dst = readWordByAddr(dstAddr = getAddrByMode(instruction, MODIFY_WORD))) >= 0) {
                                                result = dst + ((CPU.flagC >> 16) & 1);
                                                if (writeWordByAddr(dstAddr, result) >= 0) {
                                                    CPU.flagC = CPU.flagN = CPU.flagZ = result;
                                                    CPU.flagV = result & (result ^ dst);
                                                }
                                            }
                                            break;
                                        case 0005600: // SBC 0056DD
                                            //LOG_INSTRUCTION(instruction, 1, "SBC");
                                            if ((dst = readWordByAddr(dstAddr = getAddrByMode(instruction, MODIFY_WORD))) >= 0) {
                                                result = dst - ((CPU.flagC >> 16) & 1);
                                                if (writeWordByAddr(dstAddr, result) >= 0) {
                                                    CPU.flagC = CPU.flagN = CPU.flagZ = result;
                                                    CPU.flagV = (result ^ dst) & dst;
                                                }
                                            }
                                            break;
                                        case 0005700: // TST 0057DD
                                            //LOG_INSTRUCTION(instruction, 1, "TST");
                                            if ((dst = readWordByMode(instruction)) >= 0) {
                                                CPU.flagN = CPU.flagZ = dst;
                                                CPU.flagC = CPU.flagV = 0;
                                            }
                                            break;
                                        case 0006000: // ROR 0060DD
                                            //LOG_INSTRUCTION(instruction, 1, "ROR");
                                            if ((dst = readWordByAddr(dstAddr = getAddrByMode(instruction, MODIFY_WORD))) >= 0) {
                                                result = ((CPU.flagC & 0x10000) | dst) >> 1;
                                                if (writeWordByAddr(dstAddr, result) >= 0) {
                                                    CPU.flagC = (dst << 16);
                                                    CPU.flagN = CPU.flagZ = result;
                                                    CPU.flagV = result ^ (CPU.flagC >> 1);
                                                }
                                            }
                                            break;
                                        case 0006100: // ROL 0061DD
                                            //LOG_INSTRUCTION(instruction, 1, "ROL");
                                            if ((dst = readWordByAddr(dstAddr = getAddrByMode(instruction, MODIFY_WORD))) >= 0) {
                                                result = (dst << 1) | ((CPU.flagC >> 16) & 1);
                                                if (writeWordByAddr(dstAddr, result) >= 0) {
                                                    CPU.flagC = CPU.flagN = CPU.flagZ = result;
                                                    CPU.flagV = result ^ dst;
                                                }
                                            }
                                            break;
                                        case 0006200: // ASR 0062DD
                                            //LOG_INSTRUCTION(instruction, 1, "ASR");
                                            if ((dst = readWordByAddr(dstAddr = getAddrByMode(instruction, MODIFY_WORD))) >= 0) {
                                                result = (dst & 0x8000) | (dst >> 1);
                                                if (writeWordByAddr(dstAddr, result) >= 0) {
                                                    CPU.flagC = dst << 16;
                                                    CPU.flagN = CPU.flagZ = result;
                                                    CPU.flagV = CPU.flagN ^ (CPU.flagC >> 1);
                                                }
                                            }
                                            break;
                                        case 0006300: // ASL 0063DD
                                            //LOG_INSTRUCTION(instruction, 1, "ASL");
                                            if ((dst = readWordByAddr(dstAddr = getAddrByMode(instruction, MODIFY_WORD))) >= 0) {
                                                result = dst << 1;
                                                if (writeWordByAddr(dstAddr, result) >= 0) {
                                                    CPU.flagC = CPU.flagN = CPU.flagZ = result;
                                                    CPU.flagV = result ^ dst;
                                                }
                                            }
                                            break;
                                        case 0006400: // MARK 0064nn
                                            //LOG_INSTRUCTION(instruction, 8, "MARK");
                                            virtualAddress = (CPU.registerVal[7] + ((instruction & 077) << 1)) & 0xffff;
                                            if ((src = readWordByVirtual(virtualAddress | 0x10000)) >= 0) {
                                                CPU.registerVal[7] = CPU.registerVal[5];
                                                CPU.registerVal[5] = src;
                                                CPU.registerVal[6] = (virtualAddress + 2) & 0xffff;
                                            }
                                            break;
                                        case 0006500: // MFPI 0065SS
                                            //LOG_INSTRUCTION(instruction, 1, "MFPI");
                                            if (!(instruction & 0x38)) {
                                                reg = instruction & 7;
                                                if (6 !== reg || ((CPU.PSW >> 2) & 0x3000) === (CPU.PSW & 0x3000)) {
                                                    src = CPU.registerVal[reg];
                                                } else {
                                                    src = CPU.stackPointer[(CPU.PSW >> 12) & 3];
                                                }
                                                if (pushWord(src, 0) >= 0) {
                                                    CPU.flagN = CPU.flagZ = src;
                                                    CPU.flagV = 0;
                                                }
                                            } else {
                                                if ((virtualAddress = getVirtualByMode(instruction, 0)) >= 0) {
                                                    if ((CPU.PSW & 0xf000) !== 0xf000) virtualAddress &= 0xffff;
                                                    CPU.mmuMode = (CPU.PSW >> 12) & 3;
                                                    if ((src = readWordByVirtual(virtualAddress)) >= 0) {
                                                        CPU.mmuMode = (CPU.PSW >> 14) & 3;
                                                        if (pushWord(src, 0) >= 0) {
                                                            CPU.flagN = CPU.flagZ = src;
                                                            CPU.flagV = 0;
                                                        }
                                                    }
                                                }
                                            }
                                            break;
                                        case 0006600: // MTPI 0066DD
                                            //LOG_INSTRUCTION(instruction, 1, "MTPI");
                                            if ((dst = popWord()) >= 0) {
                                                if (!(CPU.MMR0 & 0xe000)) CPU.MMR1 = 026;
                                                if (!(instruction & 0x38)) {
                                                    reg = instruction & 7;
                                                    if (6 !== reg || ((CPU.PSW >> 2) & 0x3000) === (CPU.PSW & 0x3000)) {
                                                        CPU.registerVal[reg] = dst;
                                                    } else {
                                                        CPU.stackPointer[(CPU.PSW >> 12) & 3] = dst;
                                                    }
                                                    CPU.flagN = CPU.flagZ = dst;
                                                    CPU.flagV = 0;
                                                } else {
                                                    if ((virtualAddress = getVirtualByMode(instruction, 0)) >= 0) {
                                                        virtualAddress &= 0xffff;
                                                        CPU.mmuMode = (CPU.PSW >> 12) & 3;
                                                        if ((dstAddr = mapVirtualToPhysical(virtualAddress, WRITE_MODE)) >= 0) {
                                                            CPU.mmuMode = (CPU.PSW >> 14) & 3;
                                                            if (writeWordByAddr(dstAddr, dst) >= 0) {
                                                                CPU.flagN = CPU.flagZ = dst;
                                                                CPU.flagV = 0;
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                            break;
                                        case 0006700: // SXT 0067DD
                                            //LOG_INSTRUCTION(instruction, 1, "SXT");
                                            if ((dstAddr = getAddrByMode(instruction, WRITE_MODE)) >= 0) { // write word
                                                result = -((CPU.flagN >> 15) & 1);
                                                if (writeWordByAddr(dstAddr, result) >= 0) {
                                                    CPU.flagZ = result;
                                                    CPU.flagV = 0;
                                                }
                                            }
                                            break;
                                        case 0105000: // CLRB 1050DD
                                            //LOG_INSTRUCTION(instruction, 1, "CLRB");
                                            if (!(instruction & 0x38)) {
                                                CPU.registerVal[instruction & 7] &= 0xff00;
                                                CPU.flagN = CPU.flagC = CPU.flagV = CPU.flagZ = 0;
                                            } else {
                                                if ((dstAddr = getAddrByMode(instruction, WRITE_MODE | BYTE_MODE)) >= 0) { // write byte
                                                    if (writeByteByAddr(dstAddr, 0) >= 0) {
                                                        CPU.flagN = CPU.flagC = CPU.flagV = CPU.flagZ = 0;
                                                    }
                                                }
                                            }
                                            break;
                                        case 0105100: // COMB 1051DD
                                            //LOG_INSTRUCTION(instruction, 1, "COMB");
                                            if ((dst = readByteByAddr(dstAddr = getAddrByMode(instruction, MODIFY_BYTE))) >= 0) {
                                                result = ~dst;
                                                if (writeByteByAddr(dstAddr, result) >= 0) {
                                                    CPU.flagN = CPU.flagZ = result << 8;
                                                    CPU.flagC = 0x10000;
                                                    CPU.flagV = 0;
                                                }
                                            }
                                            break;
                                        case 0105200: // INCB 1052DD
                                            //LOG_INSTRUCTION(instruction, 1, "INCB");
                                            if ((dst = readByteByAddr(dstAddr = getAddrByMode(instruction, MODIFY_BYTE))) >= 0) {
                                                result = dst + 1;
                                                if (writeByteByAddr(dstAddr, result) >= 0) {
                                                    CPU.flagN = CPU.flagZ = result << 8;
                                                    CPU.flagV = (result & (result ^ dst)) << 8;
                                                }
                                            }
                                            break;
                                        case 0105300: // DECB 1053DD
                                            //LOG_INSTRUCTION(instruction, 1, "DECB");
                                            if ((dst = readByteByAddr(dstAddr = getAddrByMode(instruction, MODIFY_BYTE))) >= 0) {
                                                result = dst - 1;
                                                if (writeByteByAddr(dstAddr, result) >= 0) {
                                                    CPU.flagN = CPU.flagZ = result << 8;
                                                    CPU.flagV = ((result ^ dst) & dst) << 8;
                                                }
                                            }
                                            break;
                                        case 0105400: // NEGB 1054DD
                                            //LOG_INSTRUCTION(instruction, 1, "NEGB");
                                            if ((dst = readByteByAddr(dstAddr = getAddrByMode(instruction, MODIFY_BYTE))) >= 0) {
                                                result = -dst;
                                                if (writeByteByAddr(dstAddr, result) >= 0) {
                                                    CPU.flagC = CPU.flagN = CPU.flagZ = result << 8;
                                                    CPU.flagV = (result & dst) << 8;
                                                }
                                            }
                                            break;
                                        case 0105500: // ADCB 01055DD
                                            //LOG_INSTRUCTION(instruction, 1, "ADCB");
                                            if ((dst = readByteByAddr(dstAddr = getAddrByMode(instruction, MODIFY_BYTE))) >= 0) {
                                                result = dst + ((CPU.flagC >> 16) & 1);
                                                if (writeByteByAddr(dstAddr, result) >= 0) {
                                                    CPU.flagN = CPU.flagZ = CPU.flagC = result << 8;
                                                    CPU.flagV = (result & (result ^ dst)) << 8;
                                                }
                                            }
                                            break;
                                        case 0105600: // SBCB 01056DD
                                            //LOG_INSTRUCTION(instruction, 1, "SBCB");
                                            if ((dst = readByteByAddr(dstAddr = getAddrByMode(instruction, MODIFY_BYTE))) >= 0) {
                                                result = dst - ((CPU.flagC >> 16) & 1);
                                                if (writeByteByAddr(dstAddr, result) >= 0) {
                                                    CPU.flagN = CPU.flagZ = CPU.flagC = result << 8;
                                                    CPU.flagV = ((result ^ dst) & dst) << 8;
                                                }
                                            }
                                            break;
                                        case 0105700: // TSTB 1057DD
                                            //LOG_INSTRUCTION(instruction, 1, "TSTB");
                                            if ((dst = readByteByMode(instruction)) >= 0) {
                                                CPU.flagN = CPU.flagZ = dst << 8;
                                                CPU.flagC = CPU.flagV = 0;
                                            }
                                            break;
                                        case 0106000: // RORB 1060DD
                                            //LOG_INSTRUCTION(instruction, 1, "RORB");
                                            if ((dst = readByteByAddr(dstAddr = getAddrByMode(instruction, MODIFY_BYTE))) >= 0) {
                                                result = (((CPU.flagC & 0x10000) >> 8) | dst) >> 1;
                                                if (writeByteByAddr(dstAddr, result) >= 0) {
                                                    CPU.flagC = (dst << 16);
                                                    CPU.flagN = CPU.flagZ = (result << 8);
                                                    CPU.flagV = CPU.flagN ^ (CPU.flagC >> 1);
                                                }
                                            }
                                            break;
                                        case 0106100: // ROLB 1061DD
                                            //LOG_INSTRUCTION(instruction, 1, "ROLB");
                                            if ((dst = readByteByAddr(dstAddr = getAddrByMode(instruction, MODIFY_BYTE))) >= 0) {
                                                result = (dst << 1) | ((CPU.flagC >> 16) & 1);
                                                if (writeByteByAddr(dstAddr, result) >= 0) {
                                                    CPU.flagC = CPU.flagN = CPU.flagZ = result << 8;
                                                    CPU.flagV = (result ^ dst) << 8;
                                                }
                                            }
                                            break;
                                        case 0106200: // ASRB 1062DD
                                            //LOG_INSTRUCTION(instruction, 1, "ASRB");
                                            if ((dst = readByteByAddr(dstAddr = getAddrByMode(instruction, MODIFY_BYTE))) >= 0) {
                                                result = (dst & 0x80) | (dst >> 1);
                                                if (writeByteByAddr(dstAddr, result) >= 0) {
                                                    CPU.flagC = dst << 16;
                                                    CPU.flagN = CPU.flagZ = result << 8;
                                                    CPU.flagV = CPU.flagN ^ (CPU.flagC >> 1);
                                                }
                                            }
                                            break;
                                        case 0106300: // ASLB 1063DD
                                            //LOG_INSTRUCTION(instruction, 1, "ASLB");
                                            if ((dst = readByteByAddr(dstAddr = getAddrByMode(instruction, MODIFY_BYTE))) >= 0) {
                                                result = dst << 1;
                                                if (writeByteByAddr(dstAddr, result) >= 0) {
                                                    CPU.flagC = CPU.flagN = CPU.flagZ = result << 8;
                                                    CPU.flagV = (result ^ dst) << 8;
                                                }
                                            }
                                            break;
                                            //case 0106400: // MTPS 1064SS
                                            //    //LOG_INSTRUCTION(instruction, 1, "MTPS");
                                            //    if ((src = readByteByMode(instruction)) >= 0) {
                                            //        writePSW((CPU.PSW & 0xff00) | (src & 0xef));
                                            //    } // Temporary PDP 11/34A
                                            //    break;
                                        case 0106500: // MFPD 1065DD
                                            //LOG_INSTRUCTION(instruction, 1, "MFPD");
                                            if (!(instruction & 0x38)) {
                                                reg = instruction & 7;
                                                if (6 !== reg || ((CPU.PSW >> 2) & 0x3000) === (CPU.PSW & 0x3000)) {
                                                    src = CPU.registerVal[reg];
                                                } else {
                                                    src = CPU.stackPointer[(CPU.PSW >> 12) & 3];
                                                }
                                                if (pushWord(src, 0) >= 0) {
                                                    CPU.flagN = CPU.flagZ = src;
                                                    CPU.flagV = 0;
                                                }
                                            } else {
                                                if ((virtualAddress = getVirtualByMode(instruction, 0)) >= 0) {
                                                    CPU.mmuMode = (CPU.PSW >> 12) & 3;
                                                    if ((src = readWordByVirtual(virtualAddress | 0x10000)) >= 0) {
                                                        CPU.mmuMode = (CPU.PSW >> 14) & 3;
                                                        if (pushWord(src, 0) >= 0) {
                                                            CPU.flagN = CPU.flagZ = src;
                                                            CPU.flagV = 0;
                                                        }
                                                    }
                                                }
                                            }
                                            break;
                                        case 0106600: // MTPD 1066DD
                                            //LOG_INSTRUCTION(instruction, 1, "MTPD");
                                            if ((dst = popWord()) >= 0) {
                                                if (!(CPU.MMR0 & 0xe000)) CPU.MMR1 = 026;
                                                if (!(instruction & 0x38)) {
                                                    reg = instruction & 7;
                                                    if (6 !== reg || ((CPU.PSW >> 2) & 0x3000) === (CPU.PSW & 0x3000)) {
                                                        CPU.registerVal[reg] = dst;
                                                    } else {
                                                        CPU.stackPointer[(CPU.PSW >> 12) & 3] = dst;
                                                    }
                                                    CPU.flagN = CPU.flagZ = dst;
                                                    CPU.flagV = 0;
                                                } else {
                                                    if ((virtualAddress = getVirtualByMode(instruction, 0)) >= 0) {
                                                        CPU.mmuMode = (CPU.PSW >> 12) & 3;
                                                        if ((dstAddr = mapVirtualToPhysical(virtualAddress | 0x10000, WRITE_MODE)) >= 0) {
                                                            CPU.mmuMode = (CPU.PSW >> 14) & 3;
                                                            if (writeWordByAddr(dstAddr, dst) >= 0) {
                                                                CPU.flagN = CPU.flagZ = dst;
                                                                CPU.flagV = 0;
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                            break;
                                            //case 0106700: // MTFS 1064SS
                                            //    //LOG_INSTRUCTION(instruction, 1, "MFPS");
                                            //    src = readPSW() & 0xff;
                                            //    if (instruction & 0x38) {
                                            //        if ((dstAddr = getAddrByMode(instruction, WRITE_MODE | BYTE_MODE)) >= 0) { // write byte
                                            //            if (writeByteByAddr(dstAddr, src) >= 0) {
                                            //                CPU.flagN = CPU.flagZ = src << 8;
                                            //                CPU.flagV = 0;
                                            //            }
                                            //        }
                                            //    } else {
                                            //        if (src & 0200) src |= 0xff00;
                                            //        CPU.registerVal[instruction & 7] = src;
                                            //        CPU.flagN = CPU.flagZ = src << 8;
                                            //        CPU.flagV = 0;
                                            //    } // Temporary PDP 11/34A
                                            //    break;
                                        default:
                                            switch (instruction & 0177770) { // Single register instructions xxxxxR (and CC)
                                                case 0000200: // RTS 00020R
                                                    //LOG_INSTRUCTION(instruction, 6, "RTS");
                                                    if ((src = popWord()) >= 0) {
                                                        reg = instruction & 7;
                                                        CPU.registerVal[7] = CPU.registerVal[reg];
                                                        CPU.registerVal[reg] = src;
                                                    }
                                                    break;
                                                case 0000230: // SPL 00023N
                                                    //LOG_INSTRUCTION(instruction, 9, "SPL");
                                                    if (!(CPU.PSW & 0xc000)) {
                                                        CPU.PSW = (CPU.PSW & 0xf81f) | ((instruction & 7) << 5);
                                                        CPU.priorityReview = 2;
                                                    }
                                                    break;
                                                case 0000240: // CLR CC 00024M Part 1 without N
                                                case 0000250: // CLR CC 00025M Part 2 with N
                                                    //LOG_INSTRUCTION(instruction, 10, "CLR CC");
                                                    if (instruction & 1) CPU.flagC = 0; // CLC
                                                    if (instruction & 2) CPU.flagV = 0; // CLV
                                                    if (instruction & 4) CPU.flagZ = 1; // CLZ
                                                    if (instruction & 8) CPU.flagN = 0; // CLN
                                                    break;
                                                case 0000260: // SET CC 00026M Part 1 without N
                                                case 0000270: // SET CC 00026M Part 2 with N
                                                    //LOG_INSTRUCTION(instruction, 10, "SET CC");
                                                    if (instruction & 1) CPU.flagC = 0x10000; // SEC
                                                    if (instruction & 2) CPU.flagV = 0x8000; // SEV
                                                    if (instruction & 4) CPU.flagZ = 0; // SEZ
                                                    if (instruction & 8) CPU.flagN = 0x8000; // SEN
                                                    break;
                                                default: // Misc instructions (decode ALL remaining bits) xxxxxx
                                                    switch (instruction) {
                                                        case 0000000: // HALT 000000
                                                            //LOG_INSTRUCTION(instruction, 0, "HALT");
                                                            if (0xc000 & CPU.PSW) {
                                                                CPU.CPU_Error |= 0200;
                                                                trap(4, 46);
                                                            } else {
                                                                CPU.runState = STATE.HALT; // halt
                                                                loopCount = 0;
                                                                //LOG_PRINT();
                                                                console.log("HALT at " + CPU.registerVal[7].toString(8));
                                                            }
                                                            break;
                                                        case 0000001: // WAIT 000001
                                                            //LOG_INSTRUCTION(instruction, 0, "WAIT");
                                                            if (!interruptWaitRelease()) {
                                                                CPU.runState = STATE.WAIT; // WAIT; // Go to wait state and exit loop
                                                                if (loopCount > 1) loopCount = 0;
                                                                CPU.displayAddress = CPU.registerVal[7]; // WAIT displays current PC (think it reads next instruction)
                                                            }
                                                            break;
                                                        case 0000003: // BPT  000003
                                                            //LOG_INSTRUCTION(instruction, 0, "BPT");
                                                            trap(014, 6);
                                                            break;
                                                        case 0000004: // IOT  000004
                                                            //LOG_INSTRUCTION(instruction, 0, "IOT");
                                                            trap(020, 8);
                                                            break;
                                                        case 0000005: // RESET 000005
                                                            //LOG_INSTRUCTION(instruction, 0, "RESET");
                                                            if (!(CPU.PSW & 0xc000)) {
                                                                reset_iopage();;
                                                                CPU.runState = STATE.RESET; // reset state for special pause
                                                                loopCount = 0; // go update the lights
                                                            }
                                                            break;
                                                        case 0000002: // RTI 000002
                                                        case 0000006: // RTT 000006
                                                            //LOG_INSTRUCTION(instruction, 0, "RTT");
                                                            dstAddr = CPU.registerVal[6];
                                                            if ((virtualAddress = readWordByVirtual(dstAddr | 0x10000)) >= 0) {
                                                                dstAddr = (dstAddr + 2) & 0xffff;
                                                                if ((savePSW = readWordByVirtual(dstAddr | 0x10000)) >= 0) {
                                                                    CPU.registerVal[6] = (dstAddr + 2) & 0xffff;
                                                                    savePSW &= 0xf8ff;
                                                                    if (CPU.PSW & 0xc000) { // user / super restrictions
                                                                        // keep SPL and allow lower only for modes and register set
                                                                        savePSW = (savePSW & 0xf81f) | (CPU.PSW & 0xf8e0);
                                                                    }
                                                                    CPU.registerVal[7] = virtualAddress;
                                                                    writePSW(savePSW);
                                                                    CPU.trapMask &= ~0x10; // turn off Trace trap
                                                                    if (instruction === 2) CPU.trapMask |= CPU.PSW & 0x10; // RTI enables immediate trace
                                                                }
                                                            }
                                                            break;
                                                            //case 0000007: // MFPT 000007
                                                            //    //LOG_INSTRUCTION(instruction, 0, "MFPT");
                                                            //    CPU.registerVal[0] = 1;
                                                            //    break; // Exists on pdp 11/44 & KB11-EM
                                                        default: // We don't know this instruction
                                                            //LOG_INSTRUCTION(instruction, 11, "-unknown-");
                                                            trap(010, 48); // Illegal instruction
                                                            break;
                                                    }
                                            }
                                    }
                            }
                    }
            }
        }
    } while (--loopCount > 1 || (loopCount = ((loopCount > 0 && loopTime >= Date.now()) ? 5000 : 0))); // check clock every 5000 iterations

    if (panel.rotary2 == 1) { // Update the address lights
        if (panel.address !== CPU.displayPhysical) panel.address = updatePanel("a", panel.address, CPU.displayPhysical);
    } else {
        if (panel.address !== CPU.displayAddress) panel.address = updatePanel("a", panel.address, CPU.displayAddress);
    }
    switch (panel.rotary1) {
	case 0:
	case 1:
		if (CPU.runState == STATE.RUN) { // Update the data lights
			panel.display = updatePanel("d", panel.display, result & 0xffff); // Random data path
		} else {
			if (panel.display !== CPU.registerVal[0]) panel.display = updatePanel("d", panel.display, CPU.registerVal[0]);
		}
		break;
	case 2:
        if (panel.display !== CPU.switchRegister) panel.display = updatePanel("d", panel.display, CPU.switchRegister);
		break;
	case 3:
        if (panel.display !== CPU.displayRegister) panel.display = updatePanel("d", panel.display, CPU.displayRegister);
		break;
    }
	updateStatus(0); // update status lights (run, mode, memory mode, etc...)
}


function emulate() {
    if (CPU.runState == STATE.RUN) {
        step(9999); // wish there was a way to test for a pending javascript events :-(
	}
    if (CPU.runState == STATE.RUN) {
        setTimeout(emulate, 0); // schedule another batch of instructions
    } else {
		if (CPU.runState == STATE.RESET) {
			CPU.runState = STATE.RUN;
			setTimeout(emulate, 10); // schedule instructions after reset pause
		}
    }
}