// Javascript PDP 11/70 Emulator v1.5
// written by Paul Nankervis
// Please send suggestions, fixes and feedback to paulnank@hotmail.com
// I'm particularly interested in hearing from anyone with real experience on a PDP 11/70 front panel
//
// This code may be used freely provided the original author name is acknowledged in any modified source code
//
// http://skn.noip.me/pdp11/pdp11.html
//
// Disk routines need a clean up - specifically the RP11 stuff needs rewriting :-(
//




// Map an 18 bit unibus address to a 22 bit memory address via the unibus map (if active)

function mapUnibus(unibusAddress) {
    var idx = (unibusAddress >> 13) & 0x1f;
    if (idx < 31) {
        if (CPU.MMR3 & 0x20) {
            unibusAddress = (CPU.unibusMap[idx] + (unibusAddress & 0x1fff)) & 0x3fffff;
        }
    } else {
        unibusAddress |= IOBASE_22BIT; // top page always maps to unibus i/o page - apparently.
    }
    return unibusAddress;
}

// =========== Disk I/O support routines ===========

// getData() is called at the completion of an XMLHttpRequest request to GET disk data.
// It extracts the received data and stores it in the appropriate disk cache.

function getData(xhr, operation, meta, position, address, count) {
    var arrayBuffer, byteArray, block, word, base;
    arrayBuffer = xhr.response;
    if ((xhr.status != 0 && xhr.status != 206) || !arrayBuffer) {
        meta.postProcess(1, meta, position, address, count); // NXD - read error?
    } else {
        byteArray = new Uint8Array(arrayBuffer);
        block = ~~(position / meta.blockSize);
        for (base = 0; base < byteArray.length;) {
            if (typeof meta.cache[block] !== "undefined") {
                base += meta.blockSize << 1;
            } else {
                meta.cache[block] = [];
                for (word = 0; word < meta.blockSize; word++) {
                    if (base < byteArray.length) {
                        meta.cache[block][word] = (byteArray[base] & 0xff) | ((byteArray[base + 1] << 8) & 0xff00);
                    } else {
                        meta.cache[block][word] = 0;
                    }
                    base += 2;
                }
            }
            block++;
        }
        diskIO(operation, meta, position, address, count);
    }
}

// diskIO() moves data between memory and the disk cache. If cache blocks are undefined then
// an XMLHttpRequest request is kicked off to get the appropriate disk data from the server. 
// Operations supported are:  1: Write, 2: Read, 3: Check (corresponds with RK function codes :-) )
// position is in words and count in bytes (an allowance for tape which can do byte IO)

function diskIO(operation, meta, position, address, count) {
    var block, word, data, xhr;
    block = ~~(position / meta.blockSize);
    if (typeof meta.cache[block] !== "undefined") {
        word = position % meta.blockSize;
        while (count > 0) {
            switch (operation) {
                case 1: // Write: write from memory to cache
                case 3: // Check: compare memory with disk cache
                    data = readWordByAddr((meta.mapped ? mapUnibus(address) : address));
                    if (data < 0) {
                        meta.postProcess(2, meta, block * meta.blockSize + word, address, count); // NXM
                        return;
                    }
                    if (operation == 1) { // write: put data into disk cache
                        meta.cache[block][word] = data;
                    } else { // check: compare memory with disk cache
                        if (meta.cache[block][word] != data) {
                            meta.postProcess(3, meta, block * meta.blockSize + word, address, count); // mismatch
                            return;
                        }
                    }
                    //if (meta.increment) {
                    address += 2;
                    //}
                    count -= 2; // bytes to go.... (currently all write operations are whole words)
                    break;
                case 2: // Read: read to memory from cache
                    if (count > 1) { // tape can read odd number of bytes - of course it can. :-(
                        if (writeWordByAddr((meta.mapped ? mapUnibus(address) : address), meta.cache[block][word]) < 0) {
                            meta.postProcess(2, meta, block * meta.blockSize + word, address, count); // NXM
                            return;
                        }
                        //if (meta.increment) {
                        address += 2;
                        //}
                        count -= 2; // bytes to go....
                    } else {
                        if (writeByteByAddr((meta.mapped ? mapUnibus(address) : address), data) < 0) {
                            meta.postProcess(2, meta, block * meta.blockSize + word, address, count); // NXM
                            return;
                        }
                        //if (meta.increment) {
                        address += 1;
                        //}
                        --count; // bytes to go....
                    }
                    break;
                case 4: // accumulate a record count into the address field for tape operations
                    address = (meta.cache[block][word] << 16) | (address >> 16);
                    count -= 2; // bytes to go....
                    break;
                default:
                    panic(); // invalid operation - how did we get here?
            }
            if (++word >= meta.blockSize) {
                word = 0;
                block++;
                if (typeof meta.cache[block] === "undefined") break;
            }
        }
        position = block * meta.blockSize + word;
    }
    if (count > 0) { // I/O not complete so we need to get some data
        word = (~~(position / meta.blockSize)) * meta.blockSize; // Start word
        data = (~~((position + (count >> 1) + meta.blockSize - 1) / meta.blockSize)) * meta.blockSize; // End word
        xhr = new XMLHttpRequest();
        xhr.open("GET", meta.url, true);
        xhr.setRequestHeader("Range", "bytes=" + (word << 1) + "-" + ((data << 1) - 1));
        xhr.responseType = "arraybuffer";
        xhr.onreadystatechange = function() {
            if (xhr.readyState == xhr.DONE) {
                getData(xhr, operation, meta, position, address, count);
            }
        };
        xhr.send(null);
        return;
    }
    meta.postProcess(0, meta, position, address, count); // success
}



// =========== RK11 routines ===========

var rk11 = {
    rkds: 04700, // 017777400 Drive Status
    rker: 0, // 017777402 Error Register
    rkcs: 0200, // 017777404 Control Status
    rkwc: 0, // 017777406 Word Count
    rkba: 0, // 017777410 Bus Address
    rkda: 0, // 017777412 Disk Address
    meta: [],
    TRACKS: [406, 406, 406, 406, 406, 406, 406, 0],
    SECTORS: [12, 12, 12, 12, 12, 12, 12, 12]
};

function rk11_seekEnd(drive) {
    rk11.rkds = (drive << 13) | (rk11.rkds & 0x1ff0);
    rk11.rkcs |= 0x2000;
    return rk11.rkcs & 0x40;
}

function rk11_commandEnd(drive) {
    rk11.rkds = (drive << 13) | (rk11.rkds & 0x1ff0);
    rk11.rkcs = (rk11.rkcs & 0xfffe) | 0x80; // turn off go & set done
    return rk11.rkcs & 0x40;
}

function rk11_finish(drive) {
    if (rk11.rkcs & 0x40) {
        interrupt(0, 10, 5 << 5, 0220, rk11_commandEnd, drive);
    } else { // if interrupt not enabled just mark completed
        rk11_commandEnd(drive);
    }
}

function rk11_init() {
    rk11.rkds = 04700; // Set bits 6, 7, 8, 11  
    rk11.rker = 0; //
    rk11.rkcs = 0200;
	rk11.rkwc = 0;
	rk11.rkba = 0;
    rk11.rkda = 0;
}

function rk11_go() {
    var sector, address, count;
    var drive = (rk11.rkda >> 13) & 7;
    if (typeof rk11.meta[drive] === "undefined") {
        rk11.meta[drive] = {
            "cache": [],
            "blockSize": 65536,
            "postProcess": rk11_end,
            "drive": drive,
            "mapped": 1,
            "maxblock": rk11.TRACKS[drive] * rk11.SECTORS[drive],
            "url": "rk" + drive + ".dsk"
        };
    }
    rk11.rkcs &= ~0x2080; // turn off done bit & search complete
    rk11.rker &= ~0x03; // turn off soft errors
    if (rk11.TRACKS[drive] == 0) {
        rk11.rker |= 0x8080; // NXD
    } else {
        sector = (((rk11.rkda >> 4) & 0x1ff) * rk11.SECTORS[drive] + (rk11.rkda & 0xf));
        address = (((rk11.rkcs & 0x30)) << 12) | rk11.rkba;
        count = (0x10000 - rk11.rkwc) & 0xffff;
        switch ((rk11.rkcs >> 1) & 7) { // function code
            case 0: // controller reset
                interrupt(1, -1, 5 << 5, 0220);
				rk11_init();
                break;
            case 1: // write
            case 2: // read
            case 3: // check
                if (((rk11.rkda >> 4) & 0x1ff) >= rk11.TRACKS[drive]) {
                    rk11.rker |= 0x8040; // NXC
                    break;
                }
                if ((rk11.rkda & 0xf) >= rk11.SECTORS[drive]) {
                    rk11.rker |= 0x8020; // NXS
                    break;
                }
                sector = (((rk11.rkda >> 4) & 0x1ff) * rk11.SECTORS[drive] + (rk11.rkda & 0xf));
                address = (((rk11.rkcs & 0x30)) << 12) | rk11.rkba;
                count = (0x10000 - rk11.rkwc) & 0xffff;
                diskIO((rk11.rkcs >> 1) & 7, rk11.meta[drive], sector * 256, address, count << 1);
                return;
            case 4: // Seek - complete immediately
                rk11.rkcs |= 0x2000; // Set search complete
                interrupt(0, 20, 5 << 5, 0220, rk11_seekEnd, drive);
                break;
            case 5: // Read Check
                break;
            case 6: // Drive Reset
                rk11.rkds = 04700 | (drive << 13);
                rk11.rker = 0; //
                rk11.rkcs = 0200;
                rk11.rkda &= 0xe000; // keep drive number
                interrupt(0, 20, 5 << 5, 0220, rk11_seekEnd, drive);
                //rk11.rkcs |= 0x2000; // Set search complete
                break;
            case 7: // Write Lock - not implemented :-(
                break;
            default:
                break;
        }
    }
    rk11_finish(drive);
}


function rk11_end(err, meta, position, address, count) {
    rk11.rkba = address & 0xffff;
    rk11.rkcs = (rk11.rkcs & ~0x30) | ((address >> 12) & 0x30);
    rk11.rkwc = (0x10000 - (count >> 1)) & 0xffff;
	position = ~~(position / 256);
	rk11.rkda = (rk11.rkda & 0xe000) | ((~~(position / rk11.SECTORS[meta.drive])) << 4) | (position % rk11.SECTORS[meta.drive]);
    switch (err) {
        case 1: // read error
            rk11.rker |= 0x8100; // Report TE (Timing error)
            break;
        case 2: // NXM
            rk11.rker |= 0x8400; // NXM
            break;
        case 3: // compare error
            rk11.rker |= 0x8001; // Report TE (Write check error)
            break;
    }
    rk11_finish(meta.drive);
}


function accessRK11(physicalAddress, data, byteFlag) {
    var result;
    switch (physicalAddress & ~1) {
        case 017777400: // rk11.rkds
            result = rk11.rkds;
            break;
        case 017777402: // rk11.rker
            result = rk11.rker;
            break;
        case 017777404: // rk11.rkcs
            rk11.rkcs &= 0x3fff;
            if (rk11.rker & 0x7fff) rk11.rkcs |= 0x8000;
            if (rk11.rker & 0x7fc0) rk11.rkcs |= 0x4000;
            result = insertData(rk11.rkcs, physicalAddress, data, byteFlag);
            if (data >= 0 && result >= 0) {
                if ((rk11.rkcs ^ result) & 0x40) { // Has IE bit changed?
                    if (result & 0x40) {
                        if (!(result & 1)) {
                            interrupt(1, 0, 5 << 5, 0220);
                        }
                    } else {
                        rk11.rkcs = (rk11.rkcs & 0xfffe) | 0x80; // turn off go & set done
                        interrupt(1, -1, 5 << 5, 0220);
                    }
                }
                rk11.rkcs = (result & ~0xf080) | (rk11.rkcs & 0xf080); // Bits 7 and 12 - 15 are read only
                if (rk11.rkcs & 1) {
                    rk11.rkcs &= ~0x2080; // turn off done bit & search complete
                    rk11.rker &= ~0x03; // turn off soft errors
                    rk11_go();
                    //setTimeout(rk11_go, 10); 
                }
            }
            break;
        case 017777406: // rk11.rkwc
            result = insertData(rk11.rkwc, physicalAddress, data, byteFlag);
            if (result >= 0) rk11.rkwc = result;
            break;
        case 017777410: // rk11.rkba
            result = insertData(rk11.rkba, physicalAddress, data, byteFlag);
            if (result >= 0) rk11.rkba = result;
            break;
        case 017777412: // rk11.rkda
            result = insertData(rk11.rkda, physicalAddress, data, byteFlag);
            if (result >= 0) rk11.rkda = result;
            break;
        case 017777414: // rk11.unused
        case 017777416: // rk11.rkdb
            result = 0;
            break;
        default:
            CPU.CPU_Error |= 0x10;
            return trap(4, 134);
    }
	//console.log("RK11 Access "+physicalAddress.toString(8)+" "+data.toString(8)+" "+byteFlag.toString(8)+" -> "+result.toString(8));
    return result;
}


// =========== RL11 routines ===========

var rl11 = {
    csr: 0x81, // 017774400 Control status register
    bar: 0, // 017774402 Bus address
    dar: 0, // 017774404 Disk address
    mpr: 0, // 017774406 Multi purpose
    DAR: 0, // internal disk address
    meta: [], // sector cache
    SECTORS: [40, 40, 40, 40], // sectors per track
    TRACKS: [1024, 1024, 512, 512], // First two drives RL02 - last two RL01 - cylinders * 2
    STATUS: [0235, 0235, 035, 035] // First two drives RL02 - last two RL01
};

function rl11_commandEnd() {
    rl11.csr |= 0x81; // turn off go & set ready
    return rl11.csr & 0x40;
}

function rl11_finish(drive) {
    if (rl11.csr & 0x40) {
        interrupt(0, 10, 5 << 5, 0160, rl11_commandEnd);
    } else { // if interrupt not enabled just mark completed
        rl11_commandEnd();
    }
}

function rl11_go() {
    var sector, address, count;
    var drive = (rl11.csr >> 8) & 3;
    rl11.csr &= ~0x1; // ready bit (0!)
    if (typeof rl11.meta[drive] === "undefined") {
        rl11.meta[drive] = {
            "cache": [],
            "blockSize": 65536,
            "postProcess": rl11_end,
            "drive": drive,
            "mapped": 1,
            "maxblock": rl11.TRACKS[drive] * rl11.SECTORS[drive],
            "url": "rl" + drive + ".dsk"
        };
    }
    switch ((rl11.csr >> 1) & 7) { // function code
        case 0: // no op
            break;
        case 1: // write check
            break;
        case 2: // get status
            if (rl11.mpr & 8) rl11.csr &= 0x3f;
            rl11.mpr = rl11.STATUS[drive] | (rl11.DAR & 0100); // bit 6 Head Select bit 7 Drive Type 1=rl02
            break;
        case 3: // seek
            if ((rl11.dar & 3) == 1) {
                if (rl11.dar & 4) {
                    rl11.DAR = ((rl11.DAR + (rl11.dar & 0xff80)) & 0xff80) | ((rl11.dar << 2) & 0x40);
                } else {
                    rl11.DAR = ((rl11.DAR - (rl11.dar & 0xff80)) & 0xff80) | ((rl11.dar << 2) & 0x40);
                }
                rl11.dar = rl11.DAR;
            }
            break;
        case 4: // read header
            rl11.mpr = rl11.DAR;
            break;
        case 5: // write
            if ((rl11.dar >> 6) >= rl11.TRACKS[drive]) {
                rl11.csr |= 0x9400; // HNF
                break;
            }
            if ((rl11.dar & 0x3f) >= rl11.SECTORS[drive]) {
                rl11.csr |= 0x9400; // HNF
                break;
            }
            sector = ((rl11.dar >> 6) * rl11.SECTORS[drive]) + (rl11.dar & 0x3f);
            address = rl11.bar | ((rl11.csr & 0x30) << 12);
            count = (0x10000 - rl11.mpr) & 0xffff;
            diskIO(1, rl11.meta[drive], sector * 128, address, count << 1);
            return;
            break;
        case 6: // read
        case 7: // Read data without header check
            if ((rl11.dar >> 6) >= rl11.TRACKS[drive]) {
                rl11.csr |= 0x9400; // HNF
                break;
            }
            if ((rl11.dar & 0x3f) >= rl11.SECTORS[drive]) {
                rl11.csr |= 0x9400; // HNF
                break;
            }
            sector = ((rl11.dar >> 6) * rl11.SECTORS[drive]) + (rl11.dar & 0x3f);
            address = rl11.bar | ((rl11.csr & 0x30) << 12);
            count = (0x10000 - rl11.mpr) & 0xffff;
            diskIO(2, rl11.meta[drive], sector * 128, address, count << 1);
            return;
            break;
    }
    rl11_finish();
    //setTimeout(rl11_finish,0);
}


function rl11_end(err, meta, position, address, count) {
    var sector = ~~(position / 128);
    rl11.bar = address & 0xffff;
    rl11.csr = (rl11.csr & ~0x30) | ((address >> 12) & 0x30);
    rl11.dar = ((~~(sector / rl11.SECTORS[meta.drive])) << 6) | (sector % rl11.SECTORS[meta.drive]);
    rl11.DAR = rl11.dar;
    rl11.mpr = (0x10000 - (count >> 1)) & 0xffff;
    switch (err) {
        case 1: // read error
            rl11.csr |= 0x8400; // Report operation incomplete
            break;
        case 2: // NXM
            rl11.csr |= 0xa000; // NXM
            break;
    }
    rl11_finish();
}

function accessRL11(physicalAddress, data, byteFlag) {
    var result;
    switch (physicalAddress & ~1) {
        case 017774400: // rl11.csr
            result = insertData(rl11.csr, physicalAddress, data, byteFlag);
            if (data >= 0 && result >= 0) {
                if ((rl11.csr & 0x40) && !(result & 0x40)) { // if IE being reset then kill any pending interrupts
                    //rl11.csr |= 0x81; // turn off go & set ready
                    interrupt(1, -1, 5 << 5, 0160);
                }
                if (!(result & 0x80)) {
                    rl11.csr = (rl11.csr & ~0x3fe) | (result & 0x3fe);
                    rl11_go();
                } else {
                    if ((result & 0x40) && !(rl11.csr & 0x40)) {
                        interrupt(1, 10, 5 << 5, 0160);
                    }
                    rl11.csr = (rl11.csr & ~0x3fe) | (result & 0x3fe);
                }
            }
            break;
        case 017774402: // rl11.bar
            result = insertData(rl11.bar, physicalAddress, data, byteFlag);
            if (result >= 0) {
                rl11.bar = result & 0xfffe;
            }
            break;
        case 017774404: // rl11.dar
            result = insertData(rl11.dar, physicalAddress, data, byteFlag);
            if (result >= 0) rl11.dar = result;
            break;
        case 017774406: // rl11.mpr
            result = insertData(rl11.mpr, physicalAddress, data, byteFlag);
            if (result >= 0) rl11.mpr = result;
            break;
        default:
            CPU.CPU_Error |= 0x10;
            return trap(4, 134);
    }
    return result;
}


// =========== RP11 routines ===========

var rp11 = {
    DTYPE: [020022, 020022, 020020, 020020, 020022, 020020, 020022, 020042], // Drive type rp06, rp06, rp04, rp04...
    SECTORS: [22, 22, 22, 22, 22, 22, 22, 50], // sectors per track
    SURFACES: [19, 19, 19, 19, 19, 19, 19, 32], //
    CYLINDERS: [815, 815, 815, 815, 815, 411, 815, 630],
    meta: [], //meta data for drive
    rpcs1: 0x880, // Massbus 00 - actual register is a mix of controller and drive bits :-(
    rpwc: 0,
    rpba: 0, // rpba & rpbae
    rpda: [0, 0, 0, 0, 0, 0, 0, 0], // Massbus 05
    rpcs2: 0,
    rpds: [0x1180, 0x1180, 0x1180, 0x1180, 0, 0, 0, 0], // Massbus 01 Read only
    rper1: [0, 0, 0, 0, 0, 0, 0, 0], // Massbus 02
    // rpas: 0, // Massbus 04???
    rpla: [0, 0, 0, 0, 0, 0, 0, 0], // Massbus 07 Read only
    rpdb: 0,
    rpmr: [0, 0, 0, 0, 0, 0, 0, 0], // Massbus 03
    rpdt: [0, 0, 0, 0, 0, 0, 0, 0], // Massbus 06 Read only
    rpsn: [1, 2, 3, 4, 5, 6, 7, 8], // Massbus 10 Read only
    rpof: [0, 0, 0, 0, 0, 0, 0, 0], // Massbus 11
    rpdc: [0, 0, 0, 0, 0, 0, 0, 0], // Massbus 12
    rpcc: [0, 0, 0, 0, 0, 0, 0, 0], // Massbus 13 Read only
    rper2: [0, 0, 0, 0, 0, 0, 0, 0], // Massbus 14
    rper3: [0, 0, 0, 0, 0, 0, 0, 0], // Massbus 15
    rpec1: [0, 0, 0, 0, 0, 0, 0, 0], // Massbus 16 Read only
    rpec2: [0, 0, 0, 0, 0, 0, 0, 0], // Massbus 17 Read only
    rpcs3: 0
};


function rp11_init() {
    rp11.rpcs1 = 0x880;
    rp11.rpcs2 = 0;
    rp11.rpds = [0x11c0, 0x11c0, 0x11c0, 0x11c0, 0, 0, 0, 0];
    rp11.rpda = [0, 0, 0, 0, 0, 0, 0, 0];
    rp11.rpdc = [0, 0, 0, 0, 0, 0, 0, 0];
    rp11.rper1 = [0, 0, 0, 0, 0, 0, 0, 0];
    rp11.rper3 = [0, 0, 0, 0, 0, 0, 0, 0];
    rp11.rpas = rp11.rpwc = rp11.rpcs3 = 0;
    rp11.rpba = 0;
}


function rp11_attention(drive, flags) {
    rp11.rpas |= 1 << drive;
    rp11.rpds[drive] |= 0x8000;
    if (flags) {
        rp11.rper1[drive] |= flags;
        rp11.rpds[drive] |= 0x4000;
    }
}


//When a Data Transfer command is successfully initiated both RDY
//and DRY become negated. When a non-data transfer command is
//successfully initiated only DRY bit become negated.
//DVA should be set

function rp11_go() {
    var sector, count, drive = rp11.rpcs2 & 7;
    rp11.rpds[drive] &= 0x7fff; // turn off ATA on go bit
    if (typeof rp11.meta[drive] === "undefined") {
        rp11.meta[drive] = {
            "cache": [],
            "blockSize": 65536 * 4, //256, // 32768, // 65536 * 4,
            "postProcess": rp11_end,
            "drive": drive,
            "mapped": 0,
            "maxblock": rp11.CYLINDERS[drive] * rp11.SURFACES[drive] * rp11.SECTORS[drive],
            "url": "rp" + drive + ".dsk"
        };
    }
    switch (rp11.rpcs1 & 0x3f) { // function code
        case 01: // NULL
            return;
        case 03: // unload
            break;
        case 05: // seek
            break;
        case 07: // recalibrate
            break;
        case 011: // init
            rp11.rpds[drive] = 0x11c0; //| 0x8000;
            rp11.rpcs1 &= ~0x703f; // Turn off error bits
            rp11.rpda[drive] = 0;
            rp11.rpdc[drive] = 0;
            rp11.rpcs1 = 0x880; // ??
            return;
        case 013: // release
            return;
        case 015: // offset
            break;
        case 017: // return to centreline
            break;
        case 021: // read in preset
            // Read-in Preset - Sets the VV (volume valid) bit, clears the Desired Sector/Track Address register, clears the Desired Cylinder Address register, and clears the FMT, HCI, and ECI bits in the Offset register. Clearing the FMT bit causes the RP04 to be in IS-bit mode.
            rp11.rpdc[drive] = rp11.rpda[drive] = 0;
            rp11.rpds[drive] = 0x11c0; // |= 0x40; // set VV
            rp11.rpof[drive] = 0; // Turn off FMT 0x1000
            return;
        case 023: // pack ack
            rp11.rpds[drive] |= 0x40; // set VV
            return;
        case 031: // search
            break;
        case 061: // write
            if (rp11.rpdc[drive] >= rp11.CYLINDERS[drive] || (rp11.rpda[drive] >> 8) >= rp11.SURFACES[drive] ||
                (rp11.rpda[drive] & 0xff) >= rp11.SECTORS[drive]) {
                rp11.rper1[drive] |= 0x400; // invalid sector address
                rp11.rpcs1 |= 0xc000; // set SC & TRE
                break;
            }
            rp11.rpcs1 &= ~0x7000; // Turn error bits
            rp11.rpcs1 &= ~0x4080; // Turn TRE & ready off
            rp11.rpcs2 &= ~0x800; // Turn off NEM (NXM)
            rp11.rpds[drive] &= ~0x480; // Turn off LST & DRY
            sector = (rp11.rpdc[drive] * rp11.SURFACES[drive] + (rp11.rpda[drive] >> 8)) * rp11.SECTORS[drive] + (rp11.rpda[drive] & 0xff);
            diskIO(1, rp11.meta[drive], sector * 256, rp11.rpba, ((0x10000 - rp11.rpwc) & 0xffff) << 1);
            return;
            break;
        case 071: // read
            if (rp11.rpdc[drive] >= rp11.CYLINDERS[drive] || (rp11.rpda[drive] >> 8) >= rp11.SURFACES[drive] ||
                (rp11.rpda[drive] & 0xff) >= rp11.SECTORS[drive]) {
                rp11.rper1[drive] |= 0x400; // invalid sector address
                rp11.rpcs1 |= 0xc000; // set SC & TRE
                break;
            }
            rp11.rpcs1 &= ~0x7000; // Turn error bits
            rp11.rpcs1 &= ~0x4080; // Turn TRE & ready off
            rp11.rpcs2 &= ~0x800; // Turn off NEM (NXM)
            rp11.rpds[drive] &= ~0x480; // Turn off LST & DRY
            sector = (rp11.rpdc[drive] * rp11.SURFACES[drive] + (rp11.rpda[drive] >> 8)) * rp11.SECTORS[drive] + (rp11.rpda[drive] & 0xff);
            diskIO(2, rp11.meta[drive], sector * 256, rp11.rpba, ((0x10000 - rp11.rpwc) & 0xffff) << 1);
            return;
            break;
        default:
            panic();
            return;
            break;
    }
    interrupt(1, 12, 5 << 5, 0254, function() {
        rp11.rpds[drive] |= 0x8000; // ATA
        rp11.rpcs1 |= 0x8000; // SC no
        if (rp11.rpcs1 & 0x40) return true;
        return false;
    });
}


function rp11_end(err, meta, position, address, count) {
    var sector, block = ~~((position + 255) / 256);
    rp11.rpwc = (0x10000 - (count >> 1)) & 0xffff;
    rp11.rpba = address & 0x3fffff;
    sector = ~~(block / rp11.SECTORS[meta.drive]);
    rp11.rpda[meta.drive] = ((sector % rp11.SURFACES[meta.drive]) << 8) | (block % rp11.SECTORS[meta.drive]);
    rp11.rpdc[meta.drive] = ~~(sector / rp11.SURFACES[meta.drive]);
    if (block >= meta.maxblock) {
        rp11.rpds[meta.drive] |= 0x400; // LST
    }
    if (err) {
        rp11.rpds[meta.drive] |= 0x8000; //ATA
        rp11.rpcs1 |= 0xc000; // set SC & TRE
        switch (err) {
            case 1: // read error
                rp11.rpcs2 |= 0x200; // MXF Missed transfer
                break;
            case 2: // NXM
                rp11.rpcs2 |= 0x800; // NEM (NXM)
                break;
        }
    }
    interrupt(1, 20, 5 << 5, 0254, function() {
        rp11.rpds[meta.drive] |= 0x80; // 0x8080 must be for rp0 boot - but manual indicates no?
        rp11.rpcs1 |= 0x80; // set ready
        if (rp11.rpcs1 & 0x40) return true;
        return false;
    });
}

function accessRP11(physicalAddress, data, byteFlag) {
    var idx, result;
    idx = rp11.rpcs2 & 7;
    switch (physicalAddress & ~1) { // RH11 always there addresses
        case 017776700: // rp11.rpcs1 Control status 1
            result = (rp11.rpcs1 & ~0xb01) | ((rp11.rpba >> 8) & 0x300);
            if (rp11.rpds[idx] & 0x100) {
                result |= 0x800; // DVA depends on drive number
                if (!(rp11.rpcs1 & 0x80)) result |= 1; // go is opposite of rdy
            } else {
                result &= 0xff7f; // rdy off if no dva
            }
            rp11.rpcs1 = result;
            if (data >= 0) {
                result = insertData(result, physicalAddress, data, byteFlag);
                if (result >= 0) {
                    rp11.rpba = (rp11.rpba & 0x3cffff) | ((result << 8) & 0x30000);
                    result = (result & ~0xb880) | (rp11.rpcs1 & 0xb880);
                    if (!(result & 0x40)) interrupt(1, -1, 0, 0254); //remove pending interrupt if IE not set
                    if ((data & 0xc0) == 0xc0) interrupt(1, 8, 5 << 5, 0254); // RB:
                    rp11.rpcs1 = result;
                    if (result & 1 && (rp11.rpcs1 & 0x80)) {
                        rp11_go();
                    }
                }
            }
            break;
        case 017776702: // rp11.rpwc  Word count
            result = insertData(rp11.rpwc, physicalAddress, data, byteFlag);
            if (result >= 0) rp11.rpwc = result;
            break;
        case 017776704: // rp11.rpba  Memory address
            result = rp11.rpba & 0xffff;
            if (data >= 0) {
                result = insertData(result, physicalAddress, data, byteFlag);
                if (result >= 0) {
                    rp11.rpba = (rp11.rpba & 0x3f0000) | (result & 0xfffe); // must be even
                }
            }
            break;
        case 017776710: // rp11.rpcs2 Control status 2
            result = rp11.rpcs2;
            if (data >= 0) {
                result = insertData(result, physicalAddress, data, byteFlag);
                if (result >= 0) {
                    rp11.rpcs2 = (result & 0x3f) | (rp11.rpcs2 & 0xffc0);
                    if (result & 0x20) rp11_init();
                }
            }
            break;
        case 017776716: // rp11.rpas  Attention summary
            result = 0;
            for (idx = 0; idx < 8; idx++) {
                if (rp11.rpds[idx] & 0x8000) {
                    if (data >= 0 && (data & (1 << idx))) {
                        rp11.rpds[idx] &= 0x7fff;
                    } else {
                        result |= 1 << idx;
                    }
                }
            }
            if (data > 0) rp11.rpcs1 &= 0x7fff; // Turn off SC
            break;
        case 017776722: // rp11.rpdb  Data buffer
            result = 0;
            break;
        case 017776750: // rp11.rpbae Bus address extension
            result = (rp11.rpba >> 16) & 0x3f;
            if (data >= 0) {
                result = insertData(result, physicalAddress, data, byteFlag);
                if (result >= 0) {
                    rp11.rpba = ((result & 0x3f) << 16) | (rp11.rpba & 0xffff);
                }
            }
            break;
        case 017776752: // rp11.rpcs3 Control status 3
            // result = insertData(rp11.rpcs3, physicalAddress, data, byteFlag);
            // if (result >= 0) rp11.rpcs3 = result;
            result = 0;
            break;
        default:
            idx = rp11.rpcs2 & 7; // drive number
            if (rp11.rpds[idx] & 0x100) {
                switch (physicalAddress & ~1) { // Drive registers which may or may not be present
                    case 017776706: // rp11.rpda  Disk address
                        result = insertData(rp11.rpda[idx], physicalAddress, data, byteFlag);
                        if (result >= 0) rp11.rpda[idx] = result & 0x1f1f;
                        break;
                    case 017776712: // rp11.rpds  drive status
                        result = rp11.rpds[idx];
                        break;
                    case 017776714: // rp11.rper1 Error 1
                        result = 0; // rp11.rper1[idx];
                        break;
                    case 017776720: // rp11.rpla  Look ahead
                        result = 0; // rp11.rpla[idx];
                        break;
                    case 017776724: // rp11.rpmr  Maintenance
                        //result = insertData(rp11.rpmr[idx], physicalAddress, data, byteFlag);
                        //if (result >= 0) rp11.rpmr[idx] = result & 0x3ff;
                        result = 0;
                        break;
                    case 017776726: // rp11.rpdt  drive type read only
                        result = rp11.DTYPE[idx]; // 020022
                        break;
                    case 017776730: // rp11.rpsn  Serial number read only - lie and return drive + 1
                        result = idx + 1;
                        break;
                    case 017776732: // rp11.rpof  Offset register
                        result = insertData(rp11.rpof[idx], physicalAddress, data, byteFlag);
                        if (result >= 0) rp11.rpof[idx] = result;
                        //result = 0x1000;
                        break;
                    case 017776734: // rp11.rpdc  Desired cylinder
                        result = insertData(rp11.rpdc[idx], physicalAddress, data, byteFlag);
                        if (result >= 0) rp11.rpdc[idx] = result & 0x1ff;
                        break;
                    case 017776736: // rp11.rpcc  Current cylinder read only - lie and used desired cylinder
                        result = rp11.rpdc[idx];
                        break;
                    case 017776740: // rp11.rper2 Error 2
                        result = 0;
                        break;
                    case 017776742: // rp11.rper3 Error 3
                        result = 0; // rp11.rper3[idx];
                        break;
                    case 017776744: // rp11.rpec1 Error correction 1 read only
                        result = 0; // rp11.rpec1[idx];
                        break;
                    case 017776746: // rp11.rpec2 Error correction 2 read only
                        result = 0; //rp11.rpec2[idx];
                        break;
                    default:
                        CPU.CPU_Error |= 0x10;
                        return trap(4, 132);
                }
            } else {
                rp11.rpcs2 |= 0x1000; // NED
                rp11.rpcs1 |= 0xc000; // SC + TRE
                if (rp11.rpcs1 & 0x40) {
                    interrupt(1, 5, 5 << 5, 0254);
                }
                result = 0;
            }
    }
    return result;
}


// =========== TM11 routines ===========


var tm11 = {
    mts: 0x65, // 17772520 Status Register    6 selr 5 bot 2 wrl 0 tur
    mtc: 0x6080, // 17772522 Command Register   14-13 bpi 7 cu rdy
    mtbrc: 0, // 17772524 Byte Record Counter
    mtcma: 0, // 17772526 Current Memory Address Register
    mtd: 0, // 17772530 Data Buffer Register
    mtrd: 0, // 17772532 TU10 Read Lines
    meta: [] //meta data for drive
};

function tm11_commandEnd() {
    tm11.mts |= 1; // tape unit ready
    tm11.mtc |= 0x80;
    return tm11.mtc & 0x40;
}

function tm11_finish() {
    if (tm11.mtc & 0x40) {
        interrupt(0, 10, 5 << 5, 0224, tm11_commandEnd);
    } else { // if interrupt not enabled just mark completed
        tm11_commandEnd();
    }
}

function tm11_end(err, meta, position, address, count) {
    if (err == 0 && meta.command > 0) {
        if (address == 0 || address > 0x80000000) { // tape mark
            meta.position = position;
            tm11.mts |= 0x4000; // set EOF bit
        } else {
            switch (meta.command) {
                case 1: // read
                    meta.position = position + 2 + ((address + 1) >> 1);
                    meta.command = 0;
                    count = (0x10000 - tm11.mtbrc) & 0xffff;
                    if (count >= address || count == 0) {
                        count = address;
                        tm11.mtbrc = (tm11.mtbrc + count) & 0xffff;
                    } else {
                        tm11.mts |= 0x200; // RLE
                        tm11.mtbrc = 0;
                    }
                    address = ((tm11.mtc & 0x30) << 12) | tm11.mtcma;
                    diskIO(2, meta, position, address, count);
                    // calculate meta.position set count to reduced amount
                    return;
                case 4: // space forward
                    position = position + 2 + ((address + 1) >> 1);
                    meta.position = position;
                    tm11.mtbrc = (tm11.mtbrc + 1) & 0xffff;
                    if (tm11.mtbrc) {
                        diskIO(4, meta, position, 0, 4);
                        return;
                    }
                    break;
                case 5: // space reverse
                    position = position - 4 - ((address + 1) >> 1);
                    meta.position = position;
                    tm11.mtbrc = (tm11.mtbrc + 1) & 0xffff;
                    if (tm11.mtbrc) {
                        if (position > 0) {
                            diskIO(4, meta, position - 2, 0, 4);
                            return;
                        }
                    }
                    break;
                default:
                    panic();
            }
        }
    }
    if (meta.command == 0) {
		tm11.mtbrc = (tm11.mtbrc - count) & 0xffff;
        tm11.mtcma = address & 0xffff;
        tm11.mtc = (tm11.mtc & ~0x30) | ((address >> 12) & 0x30);
    }
    switch (err) {
        case 1: // read error
            tm11.mts |= 0x100; // Bad tape error
            break;
        case 2: // NXM
            tm11.mts |= 0x80; // NXM
            break;
    }
    tm11_finish();
}

function tm11_init() {
	var i;
    tm11.mts = 0x65; //  6 selr 5 bot 2 wrl 0 tur
    tm11.mtc = 0x6080; //  14-13 bpi 7 cu rdy
	for (i=0; i<8; i++) {
		if (typeof tm11.meta[i] !== "undefined") {
			tm11.meta[i].position == 0;
		}
	}
}

function tm11_go() {
    var sector, address, count;
    var drive = (tm11.mtc >> 8) & 3;
    tm11.mtc &= ~0x81; // ready bit (7!) and go (0)
    tm11.mts &= 0x04fe; // turn off tape unit ready
    if (typeof tm11.meta[drive] === "undefined") {
        tm11.meta[drive] = {
            "cache": [],
            "blockSize": 65536,
            "postProcess": tm11_end,
            "drive": drive,
            "mapped": 1,
            "maxblock": 0,
            "position": 0,
            "command": 0,
            "url": "tm" + drive + ".tap"
        };
    }
    tm11.meta[drive].command = (tm11.mtc >> 1) & 7;
	//console.log("TM11 Function "+(tm11.meta[drive].command).toString(8)+" "+tm11.mtc.toString(8)+" "+tm11.mts.toString(8)+" @ "+tm11.meta[drive].position.toString(8));
    switch (tm11.meta[drive].command) { // function code
        case 0: // off-line
            break;
        case 1: // read
            diskIO(4, tm11.meta[drive], tm11.meta[drive].position, 0, 4);
            return;
        case 2: // write
        case 3: // write end of file
        case 6: // write with extended IRG
            break;
        case 4: // space forward
            diskIO(4, tm11.meta[drive], tm11.meta[drive].position, 0, 4);
            return;
        case 5: // space reverse
            if (tm11.meta[drive].position > 0) {
                diskIO(4, tm11.meta[drive], tm11.meta[drive].position - 2, 0, 4);
                return;
            }
            break;
        case 7: // rewind
            tm11.meta[drive].position = 0;
            tm11.mts |= 0x20; // set BOT
            break;
        default:
            break;
    }
    tm11_finish();
}

function accessTM11(physicalAddress, data, byteFlag) {
    var result, drive = (tm11.mtc >> 8) & 3;
    switch (physicalAddress & ~1) {
        case 017772520: // tm11.mts
			tm11.mts &= ~0x20;  // turn off BOT
			if (typeof tm11.meta[(tm11.mtc >> 8) & 3] !== "undefined") {
				if (tm11.meta[(tm11.mtc >> 8) & 3].position == 0) {
					tm11.mts |= 0x20;  // turn on BOT
				}
			}
            result = tm11.mts;
            break;
        case 017772522: // tm11.mtc
            tm11.mtc &= 0x7fff; // no err bit
            if (tm11.mts & 0xff80) tm11.mtc |= 0x8000;
            result = insertData(tm11.mtc, physicalAddress, data, byteFlag);
            if (data >= 0 && result >= 0) {
                if ((tm11.mtc & 0x40) && !(result & 0x40)) { // if IE being reset then kill any pending interrupts
                    interrupt(1, -1, 5 << 5, 0224);
                }
                if (result & 0x1000) { //init
                    tm11.mts = 0x65; //  6 selr 5 bot 2 wrl 0 tur
                    tm11.mtc = 0x6080; //  14-13 bpi 7 cu rdy
                }
                if ((tm11.mtc & 0x80) && (result & 0x1)) {
                    tm11.mtc = (tm11.mtc & 0x80) | (result & 0xff7f);
                    tm11_go();
                } else {
                    if ((result & 0x40) && (tm11.mtc & 0xc0) == 0x80) {
                        interrupt(1, 10, 5 << 5, 0224);
                    }
                    tm11.mtc = (tm11.mtc & 0x80) | (result & 0xff7f);
                }
            }
            break;
        case 017772524: // tm11.mtbrc
            result = insertData(tm11.mtbrc, physicalAddress, data, byteFlag);
            if (result >= 0) tm11.mtbrc = result;
            break;
        case 017772526: // tm11.mtcma
            result = insertData(tm11.mtcma, physicalAddress, data, byteFlag);
            if (result >= 0) tm11.mtcma = result;
            break;
        case 017772530: // tm11.mtd
        case 017772532: // tm11.mtrd
            result = 0;
            break;
        default:
            CPU.CPU_Error |= 0x10;
            return trap(4, 134);
    }
	//console.log("TM11 Access "+physicalAddress.toString(8)+" "+data.toString(8)+" "+byteFlag.toString(8)+" -> "+result.toString(8));
    return result;
}


// =========== KW11 routines ===========

var kw11 = {
    init: 0,
    csr: 0
};


function kw11_interrupt() { // Every 20 ms (50 Hz) set clock flag and schedule an interrupt
    kw11.csr |= 0x80;
    if ((CPU.runState != STATE.HALT) && (kw11.csr & 0x40)) {
        interrupt(1, 0, 6 << 5, 0100);
    }
}

// =========== Console (tty) data ===========

var tty = {
    rbufQueue: [],
    rbuf: 0,
    rcsr: 0,
    xbuf: 0,
    xcsr: 0200,
    delCode: 127,
    del: 0
};

function tty_xcsr() {
    tty.xcsr |= 0x80;
    if (tty.xcsr & 0x40) return true;
    return false;
}

function tty_rbuf() {
    if (!(tty.rcsr & 0x80)) {
        if (tty.rbufQueue.length > 0) {
            tty.rbuf = tty.rbufQueue.shift();
            tty.rcsr |= 0x80;
            if (tty.rcsr & 0x40) interrupt(1, 0, 4 << 5, 060);
        }
    }
}

// Initialize unibus things for a reset instruction

function reset_iopage() {
    CPU.PIR = 0;
    writePSW(0);
    CPU.stackLimit = 0xff;
    CPU.CPU_Error = 0;
    CPU.interruptQueue = [];
    CPU.MMR0 = CPU.MMR1 = CPU.MMR2 = CPU.MMR3 = CPU.mmuEnable = 0;
    CPU.mmuLastMode = 0;
    tty.rcsr = 0;
    tty.xcsr = 0200;
    tty.rbufQueue = [];
    kw11.csr = 0;
    rk11_init();
    rl11.csr = 0x80;
    rp11_init();
	tm11_init();

}

// Update a word with new byte or word data allowing for odd addressing

function insertData(original, physicalAddress, data, byteFlag) {
    if (physicalAddress & 1) {
        if (!byteFlag) {
            return trap(4, 122); // trap word access to odd addresses
        }
        if (data >= 0) {
            data = ((data << 8) & 0xff00) | (original & 0xff);
        } else {
            data = original;
        }
    } else {
        if (data >= 0) {
            if (byteFlag) {
                data = (original & 0xff00) | (data & 0xff);
            }
        } else {
            data = original;
        }
    }
    return data;
}

// Access to the unibus page - data is positive for a write or negative for a read

function access_iopage(physicalAddress, data, byteFlag) {
    var result, idx;
    switch (physicalAddress & ~077) {
        case 017777700: // 017777700 - 017777777
            switch (physicalAddress & ~1) {
                case 017777776: // PSW
                    result = insertData(readPSW(), physicalAddress, data, byteFlag);
                    if (result >= 0 && data >= 0) {
                        writePSW(result);
                        return -1; // Kludge - signals no further processing to prevent changes to PSW
                    }
                    break;
                case 017777774: // stack limit
                    result = insertData(CPU.stackLimit, physicalAddress, data, byteFlag);
                    if (result >= 0) {
                        if (data >= 0) {
                            CPU.stackLimit = result | 0xff; // Use stack limit with lower byte bits set
                        }
                        result &= 0xff00;
                    }
                    break;
                case 017777772: // PIR
                    result = insertData(CPU.PIR, physicalAddress, data, byteFlag);
                    if (result >= 0 && data >= 0) {
                        result &= 0xfe00;
                        if (result) { // Need to calculate priority level from priority mask
                            idx = result >> 9;
                            do {
                                result += 0x22;
                            } while (idx >>= 1);
                        }
                        CPU.PIR = result;
                        if ((result & 0xe0) > (CPU.PSW & 0xe0)) {
                            CPU.priorityReview = 1; // Schedule an interrupt priority review if required
                        }
                    }
                    break;
                case 017777766: // CPU error
                    if (CPU.cpuType !== 70) return trap(4, 222);
                    result = insertData(CPU.CPU_Error, physicalAddress, data, byteFlag);
                    if (result >= 0 && data >= 0) {
                        result = CPU.CPU_Error = 0; // Always writes as zero?
                    }
                    break;
                case 017777764: // System I/D
                    if (CPU.cpuType !== 70) return trap(4, 224);
                    result = insertData(1, physicalAddress, data, byteFlag);
                    break;
                case 017777762: // Upper size
                    if (CPU.cpuType !== 70) return trap(4, 226);
                    result = insertData(0, physicalAddress, data, byteFlag);
                    break;
                case 017777760: // Lower size
                    if (CPU.cpuType !== 70) return trap(4, 228);
                    result = insertData((MAX_MEMORY >> 6) - 1, physicalAddress, data, byteFlag);
                    break;
                case 017777770: // Microprogram break
                    if (data >= 0 && !(physicalAddress & 1)) data &= 0xff; // Required for KB11-CM without MFPT instruction
                case 017777756: //
                case 017777754: //
                case 017777752: // Hit/miss
                case 017777750: // Maintenance
                case 017777746: // Cache control
                case 017777744: // Memory system error
                case 017777742: // High error address
                case 017777740: // Low error address
                    if (CPU.cpuType !== 70) return trap(4, 232);
                    idx = (physicalAddress - 017777740) >> 1;
                    result = insertData(CPU.controlReg[idx], physicalAddress, data, byteFlag);
                    if (result >= 0) {
                        if ((physicalAddress & 1) == 017777746) result = 017;
                        if ((physicalAddress & 1) == 017777742) result = 03;
                        if ((physicalAddress & 1) == 017777740) result = 0177740;
                        CPU.controlReg[idx] = result;
                    }
                    break;
                case 017777716: // User and Super SP - note the use of odd word addresses requiring return
                    if (physicalAddress & 1) {
                        if ((CPU.PSW >> 14) & 3 == 3) { // User Mode SP
                            if (data >= 0) CPU.registerVal[6] = data;
                            result = CPU.registerVal[6];
                        } else {
                            if (data >= 0) CPU.stackPointer[3] = data;
                            result = CPU.stackPointer[3];
                        }
                    } else {
                        if ((CPU.PSW >> 14) & 3 == 1) { // Super Mode SP
                            if (data >= 0) CPU.registerVal[6] = data;
                            result = CPU.registerVal[6];
                        } else {
                            if (data >= 0) CPU.stackPointer[1] = data;
                            result = CPU.stackPointer[1];
                        }
                    }
                    return result; // special exit to allow for odd address word access
                case 017777714:
                case 017777712:
                case 017777710: // Register set 1
                    idx = physicalAddress & 7;
                    if (CPU.PSW & 0x800) {
                        if (data >= 0) CPU.registerVal[idx] = data;
                        result = CPU.registerVal[idx];
                    } else {
                        if (data >= 0) CPU.registerAlt[idx] = data;
                        result = CPU.registerAlt[idx];
                    }
                    return result; // special exit to allow for odd address word access
                case 017777706: // Kernel SP & PC
                    if (physicalAddress & 1) {
                        if (data >= 0) CPU.registerVal[7] = data;
                        result = CPU.registerVal[7];
                    } else {
                        if ((CPU.PSW >> 14) & 3 == 0) { // Kernel Mode
                            if (data >= 0) CPU.registerVal[6] = data;
                            result = CPU.registerVal[6];
                        } else {
                            if (data >= 0) CPU.stackPointer[0] = data;
                            result = CPU.stackPointer[0];
                        }
                    }
                    return result; // special exit to allow for odd address word access
                case 017777704:
                case 017777702:
                case 017777700: // Register set 0
                    idx = physicalAddress & 7;
                    if (CPU.PSW & 0x800) {
                        if (data >= 0) CPU.registerAlt[idx] = data;
                        result = CPU.registerAlt[idx];
                    } else {
                        if (data >= 0) CPU.registerVal[idx] = data;
                        result = CPU.registerVal[idx];
                    }
                    return result; // special exit to allow for odd address word access
                default:
                    CPU.CPU_Error |= 0x10;
                    return trap(4, 124);
            }
            break;
        case 017777600: // 017777600 - 017777677 MMU user mode 3 Map
            idx = (physicalAddress >> 1) & 037;
            if (idx <= 15) {
                result = insertData(CPU.mmuPDR[3][idx], physicalAddress, data, byteFlag);
                if (result >= 0) {
                    CPU.mmuPDR[3][idx] = result & 0xff0f;
                }
            } else {
                idx &= 0xf;
                result = insertData(CPU.mmuPAR[3][idx], physicalAddress, data, byteFlag);
                if (result >= 0) {
                    CPU.mmuPAR[3][idx] = result;
                    CPU.mmuPDR[3][idx] &= 0xff0f;
                }
            }
            break;
        case 017777500: // 017777500 - 017777577 MMR0 MMR1 MMR2 Console KW11
            switch (physicalAddress & ~1) {
                case 017777576: // MMR2
                    result = insertData(CPU.MMR2, physicalAddress, data, byteFlag);
                    if (result >= 0) {
                        CPU.MMR2 = result;
                    }
                    break;
                case 017777574: // MMR1
                    result = CPU.MMR1;
                    if (result & 0xff00) result = ((result << 8) | (result >> 8)) & 0xffff;
                    break;
                case 017777572: // MMR0
                    if (!(CPU.MMR0 & 0xe000)) {
                        CPU.MMR0 = (CPU.MMR0 & 0xf381) | (CPU.mmuLastMode << 5) | (CPU.mmuLastPage << 1);
                    }
                    result = insertData(CPU.MMR0, physicalAddress, data, byteFlag);
                    if (result >= 0 && data >= 0) {
                        CPU.MMR0 = result &= 0xf381;
                        CPU.mmuLastMode = (result >> 5) & 3;
                        CPU.mmuLastPage = (result >> 1) & 0xf;
                        if (result & 0x101) {
                            if (result & 0x1) {
                                CPU.mmuEnable = READ_MODE | WRITE_MODE;
                            } else {
                                CPU.mmuEnable = WRITE_MODE;
                            }
                        } else {
                            CPU.mmuEnable = 0;
                        }
                    }
                    break;
                case 017777570: // console panel display/switch;
                    if (data < 0) {
                        result = CPU.switchRegister & 0xffff;
                    } else {
                        result = insertData(CPU.displayRegister, physicalAddress, data, byteFlag);
                        if (result >= 0) CPU.displayRegister = result;
                    }
                    break;
                case 017777566: // console tty xbuf
                    result = insertData(tty.xbuf, physicalAddress, data, byteFlag);
                    if (result >= 0 & data >= 0) {
                        tty.xbuf = result &= 0x7f;
                        if (result) {
                            putchar(result);
                        }
                        if (tty.xcsr & 0x40) {
                            tty.xcsr &= ~0x80;
                            interrupt(1, 10, 4 << 5, 064, tty_xcsr);
                        } else {
                            tty.xcsr |= 0x80;
                        }
                    }
                    break;
                case 017777564: // console tty xcsr
                    result = insertData(tty.xcsr, physicalAddress, data, byteFlag);
                    if (result >= 0 && data >= 0) {
                        if (((tty.xcsr ^ result) & 0x40)) { // IE change?
                            if (result & 0x40) {
                                if (tty.xcsr & 0x80) {
                                    interrupt(1, 0, 4 << 5, 064);
                                }
                            } else {
                                tty.xcsr |= 0x80;
                                interrupt(1, -1, 4 << 5, 064);
                            }
                            tty.xcsr = (tty.xcsr & 0x80) | (result & 0x40);
                        }
                    }
                    break;
                case 017777562: // console tty rbuf
                    result = insertData(tty.rbuf, physicalAddress, data, byteFlag);
                    if (result >= 0 && data < 0) {
                        if (tty.rcsr & 0x80) {
                            tty.rcsr &= ~0x80;
                            if (tty.rbufQueue.length > 0) {
                                setTimeout(tty_rbuf, 55);
                            }
                        }
                    }
                    break;
                case 017777560: // console tty rcsr
                    result = insertData(tty.rcsr, physicalAddress, data, byteFlag);
                    if (result >= 0 && data >= 0) {
                        if (((tty.rcsr ^ result) & 0x40)) { // IE change?
                            if (!(result & 0x40)) {
                                interrupt(1, -1, 4 << 5, 060);
                            }
                        }
                        tty.rcsr = (tty.rcsr & 0x80) | (result & 0x40);
                    }
                    break;
                case 017777546: // kw11.csr
                    result = insertData(kw11.csr, physicalAddress, data, byteFlag);
                    if (result >= 0 && data >= 0) {
                        kw11.csr = result & ~0200;
                        if (!(kw11.csr & 0x40)) {
                            interrupt(1, -1, 6 << 5, 0100);
                        }
                        if (kw11.init < 1) {
                            kw11.init = 1;
                            setInterval(kw11_interrupt, 20); // initialize interrupts for every 20ms (50Hz)
                        }
                    }
                    break;
                default:
                    CPU.CPU_Error |= 0x10;
                    return trap(4, 126);
            }
            break;
        case 017777400: // 017777400 - 017777477 rk11 controller
            result = accessRK11(physicalAddress, data, byteFlag);
            break;
        case 017776700: // 017777600 - 017777677 rp11 controller
            if (physicalAddress <= 017776753) {
                result = accessRP11(physicalAddress, data, byteFlag);
            } else {
                CPU.CPU_Error |= 0x10;
                return trap(4, 134);
            }
            break;
        case 017774400: // 017774400 - 017774477 rl11 controller
            result = accessRL11(physicalAddress, data, byteFlag);
            break;
        case 017772500: // 017772500 - 017772577 MMR3
            switch (physicalAddress & ~1) {
                case 017772516: // MMR3 - UB 22 x K S U
                    result = insertData(CPU.MMR3, physicalAddress, data, byteFlag);
                    if (result >= 0 & data >= 0) {
                        if (CPU.cpuType != 70) result &= ~0x30; // don't allow 11/45 to do 22 bit or use unibus map
                        CPU.MMR3 = result;
                    }
                    break;
                default:
                    result = accessTM11(physicalAddress, data, byteFlag);
                    break;
            }
            break;
        case 017772300: // 017772300 - 017772377 MMU kernel mode 0 Map
            idx = (physicalAddress >> 1) & 037;
            if (idx <= 15) {
                result = insertData(CPU.mmuPDR[0][idx], physicalAddress, data, byteFlag);
                if (result >= 0) {
                    CPU.mmuPDR[0][idx] = result & 0xff0f;
                }
            } else {
                idx &= 0xf;
                result = insertData(CPU.mmuPAR[0][idx], physicalAddress, data, byteFlag);
                if (result >= 0) {
                    CPU.mmuPAR[0][idx] = result;
                    CPU.mmuPDR[0][idx] &= 0xff0f;
                }
            }
            break;
        case 017772200: // 017772200 - 017772277 MMU super mode 1 Map
            idx = (physicalAddress >> 1) & 037;
            if (idx <= 15) {
                result = insertData(CPU.mmuPDR[1][idx], physicalAddress, data, byteFlag);
                if (result >= 0) {
                    CPU.mmuPDR[1][idx] = result & 0xff0f;
                }
            } else {
                idx &= 0xf;
                result = insertData(CPU.mmuPAR[1][idx], physicalAddress, data, byteFlag);
                if (result >= 0) {
                    CPU.mmuPAR[1][idx] = result;
                    CPU.mmuPDR[1][idx] &= 0xff0f;
                }
            }
            break;
        case 017770300: // 017770300 - 017770377 Unibus Map
        case 017770200: // 017770200 - 017770277 Unibus Map
            if (CPU.cpuType != 70) return trap(4, 234);
            idx = (physicalAddress >> 2) & 0x1f;
            result = CPU.unibusMap[idx];
            if (physicalAddress & 02) result = (result >> 16) & 0x803f; // Low six bits plus top bit (!)
            result = insertData(result & 0xffff, physicalAddress, data, byteFlag);
            if (result >= 0 && data >= 0) {
                if (physicalAddress & 02) {
                    CPU.unibusMap[idx] = ((result & 0x803f) << 16) | (CPU.unibusMap[idx] & 0xfffe);
                } else {
                    CPU.unibusMap[idx] = (CPU.unibusMap[idx] & 0x803f0000) | (result & 0xfffe);
                }
            }
            break;
        default:
            CPU.CPU_Error |= 0x10;
            return trap(4, 142);
    }
    if (byteFlag && result >= 0) { // Make any required byte adjustment to the return result
        if ((physicalAddress & 1)) {
            result = result >> 8;
        } else {
            result &= 0xff;
        }
    }
    return result;
}