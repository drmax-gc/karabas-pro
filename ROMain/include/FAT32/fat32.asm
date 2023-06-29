			ifndef __FAT32_ASM__
			define __FAT32_ASM__

			include "memory.asm"

			define FAT32_ENABLE_CP866

			module fat32

FAT32_SIGNATURE		equ $aa55
BYTES_PER_SECTOR	equ 512
DIRENT_SIZE		equ 32

; File attributes
FA_READONLY		equ 1 << 0
FA_HIDDEN		equ 1 << 1
FA_SYSTEM		equ 1 << 2
FA_VOLUMEID		equ 1 << 3
FA_DIRECTORY		equ 1 << 4
FA_ARCHIVE		equ 1 << 5
FA_LFN			equ FA_READONLY | FA_HIDDEN | FA_SYSTEM | FA_VOLUMEID

; Структура boot-сектора
BS_jmpBoot		equ 0
BS_OEMName		equ 3
BPB_BytsPerSec		equ 11
BPB_SecPerClus		equ 13
BPB_RsvdSecCnt		equ 14
BPB_NumFATs		equ 16
BPB_RootEntCnt		equ 17
BPB_TotSec16		equ 19
BPB_Media		equ 21
BPB_FATSz16		equ 22
BPB_SecPerTrk		equ 24
BPB_NumHeads		equ 26
BPB_HiddSec		equ 28
BPB_TotSec32		equ 32
BPB_FATSz32		equ 36
BPB_RootClus		equ 44
BS_FilSysType		equ 54
BS_FilSysType32		equ 82
MBR_Table		equ 446
BS_55AA			equ 510

			struct FAT32_DIRENT
DIR_Name		ds 11, 0	; Short file name (SFN) of the object
DIR_Attr		db 0		; File attribute in combination of following flags.
					; Upper 2 bits are reserved and must be zero.
					; 0x01: ATTR_READ_ONLY (Read-only)
					; 0x02: ATTR_HIDDEN (Hidden)
					; 0x04: ATTR_SYSTEM (System)
					; 0x08: ATTR_VOLUME_ID (Volume label)
					; 0x10: ATTR_DIRECTORY (Directory)
					; 0x20: ATTR_ARCHIVE (Archive)
					; 0x0F: ATTR_LONG_FILE_NAME (LFN entry)
DIR_NTRes		db 0		; Optional flags that indicates case information of the SFN.
					; 0x08: Every alphabet in the body is low-case.
					; 0x10: Every alphabet in the extensiton is low-case.
DIR_CrtTimeTenth	db 0		; Optional sub-second information corresponds to DIR_CrtTime. The time resolution of DIR_CrtTime is 2 seconds, so that this field gives a count of sub-second and its valid value range is from 0 to 199 in unit of 10 miliseconds. If not supported, set zero and do not change afterwards.
DIR_CrtTime		dw 0		; Optional file creation time. If not supported, set zero and do not change afterwards.
DIR_CrtDate		dw 0		; Optional file creation date. If not supported, set zero and do not change afterwards.
DIR_LstAccDate		dw 0		; Optional last accesse date. There is no time information about last accesse time, so that the resolution of last accesse time is 1 day. If not supported, set zero and do not change afterwards.
DIR_FstClusHI		dw 0		; Upeer part of cluster number. Always zero on the FAT12/16 volume.
DIR_WrtTime		dw 0		; Last time when any change is made to the file (typically on closeing).
DIR_WrtDate		dw 0		; Last data when any change is made to the file (typically on closeing).
DIR_FstClusLO		dw 0		; Lower part of cluster number. Always zero if the file size is zero.
DIR_FileSize		dd 0		; Size of the file in unit of byte. Not used when it is a directroy and the value must be always zero.
			ends

			struct FAT32_DWORD
lo			dw $0000
hi			dw $0000
			ends

			struct FAT32_FS
buffer			ds BYTES_PER_SECTOR, 0
fat_cache		ds BYTES_PER_SECTOR, 0
fat_begin_lba		FAT32_DWORD
cluster_begin_lba	FAT32_DWORD
rootdir_cluster		FAT32_DWORD
cached_lba		FAT32_DWORD
sectors_per_fat		FAT32_DWORD
sectors_per_cluster	db 0
; TODO: move to FAT32_DIR
rd_ptr			dw 0
rd_entries_left		db 0
			ends
			
			struct FAT32_FILE
sectors_left		db 0
cluster_lba		FAT32_DWORD
first_cluster		FAT32_DWORD
current_cluster		FAT32_DWORD
fsize			FAT32_DWORD
fpos			FAT32_DWORD
			ends

			struct FAT32_DIR
dirfile			FAT32_FILE
; TODO
;rd_ptr			dw 0
;rd_entries_left	db 0
			ends

code

init			; call sdcard_zc.init: ret nz

; read MBR and validate filesystem
mount			; read first sector
			ld bc, 0: ld d, c: ld e, c
			ld hl, fs.buffer: call __read_sector: ret nz

			; check filesystem type
			ld a, (fs.buffer + MBR_Table + 4)
			cp $0b: jr z, .ok
			cp $0c: ret nz

			; check signature
.ok			ld hl, (fs.buffer + BS_55AA)
			ld de, FAT32_SIGNATURE: sbc hl, de: ret nz

			; get first partition lba
			ld hl, fs.buffer + MBR_Table + 4 + 4
			ld de, fs.fat_begin_lba
			call __dwcpy ; bcde = (fs.fat_begin_lba)
			; read sector at LBA = BCDE
			ld hl, fs.buffer: call __read_sector: ret nz

			; check signature
			ld hl, (fs.buffer + BS_55AA)
			ld de, FAT32_SIGNATURE: sbc hl, de: ret nz

			; ensure that sector size is 512 bytes // Word @ offset 11: BPB_BytsPerSec
			ld hl, (fs.buffer + BPB_BytsPerSec)
			ld de, BYTES_PER_SECTOR: sbc hl, de: ret nz

			; ensure that there is two FAT copies // Byte @ offset 16: BPB_NumFATs
			ld a, (fs.buffer + BPB_NumFATs)
			cp 2: ret nz

			; Word @ offset 17: BPB_RootEntCount == 0
			; Word @ offset 19: BPB_TotSec16 == 0
			; Word @ offset 22: BPB_FATSz16 == 0
			; Long @ offset 32: BPB_TotSec != 0

			; get number of sectors per cluster // Byte @ offset 13: BPB_SecPerClus
			; TODO: ensure that BPB_SecPerClus is actually a power of two
			ld a, (fs.buffer + BPB_SecPerClus)
			ld (fs.sectors_per_cluster), a

			; get number of reserved sectors // Word @ offset 14: BPB_RsvdSecCnt
			ld hl, (fs.buffer + BPB_RsvdSecCnt)

			; calculate FAT start address and cluster_begin_lba
			; DWORD fat_begin_lba = Partition_LBA_Begin + BPB_RsvdSecCnt
			ld de, (fs.fat_begin_lba.lo)
			add hl, de
			ld (fs.fat_begin_lba.lo), hl

			ld de, (fs.fat_begin_lba.hi)
			jr nc, $+3: inc de
			ld (fs.fat_begin_lba.hi), de

			push de
			push hl

			; get number of sectors per FAT // Long @ offset 36: BPB_FATSz32
			ld hl, (fs.buffer + BPB_FATSz32)
			ld (fs.sectors_per_fat.lo), hl
			add hl, hl

			ex de, hl
			ld hl, (fs.buffer + BPB_FATSz32 + 2)
			ld (fs.sectors_per_fat.hi), hl
			adc hl, hl
			ex de, hl

			; DWORD cluster_begin_lba = fat_begin_lba + (Number_of_FATs * BPB_FATSz32)
			pop bc
			add hl, bc: ld (fs.cluster_begin_lba.lo), hl
			ex de, hl
			pop bc
			adc hl, bc: ld (fs.cluster_begin_lba.hi), hl

			; get root dir cluster number // Long @ offset 44: BPB_RootClus
			ld hl, fs.buffer + BPB_RootClus
			ld de, fs.rootdir_cluster
			call __dwcpy

			; invalidate cache
			; ld a, $ff: ld (fs.cached_lba.hi + 1), a

			; all done
			xor a: ret


; map cluster number to its LBA address (sector number)
; lba_addr = cluster_begin_lba + (cluster_number - 2) * sectors_per_cluster
get_cluster_lba		; in: bcde = 28-bit cluster number
			; out: bcde = cluster lba address
			push hl
			ld h, b: ld l, c: ex de, hl ; dehl = cluster
			ld bc, -2: add hl, bc: jr c, .mul: dec de ; dehl = cluster - 2

			; return cluster_begin_lba if cluster < 2
			bit 7, d: jr nz, .return_root

			; pow2 multiply
.mul			ld a, (fs.sectors_per_cluster)
			srl a: jr z, .one
.loop			add hl, hl: rl e: rl d ; dehl *= 2 ; 25t
			rra: jr nc, .loop ; dehl = (dehl = cluster - 2) * sectors_per_cluster

.one			ld bc, (fs.cluster_begin_lba.lo)
			add hl, bc
			ex de, hl
			ld bc, (fs.cluster_begin_lba.hi)
			adc hl, bc

			; hlde = fs.cluster_begin_lba + (cluster - 2) * sectors_per_cluster
			ld b, h: ld c, l ; bcde = result
			pop hl
			ret

.return_root		ld de, (fs.cluster_begin_lba.lo)
			ld bc, (fs.cluster_begin_lba.hi)
			pop hl
			ret


get_next_cluster	; in: bcde = current cluster
			; out: bcde = next cluster
			push hl
			ld h, b: ld l, c: ex de, hl ; dehl = cluster

			; addr = cluster * 4 / BYTES_PER_SECTOR
			; = cluster * 2 / 256
			add hl, hl: rl e: rl d ; dehl <<= 1
			push hl
			ld l, h: ld h, e: ld e, d: ld d, 0: rl d ; dehl >>= 8 w/CY

			; add fat_begin_address
			ld bc, (fs.fat_begin_lba.lo)
			add hl, bc: ex de, hl

			ld bc, (fs.fat_begin_lba.hi)
			adc hl, bc: ld b, h: ld c, l
			; bcde = lba addr

			; check cache
			ld hl, fs.cached_lba
			call __eq_bcde
			jr z, .cache_valid

			ld hl, fs.fat_cache
			call __read_sector

.cache_valid		pop hl
			jr nz, .error
			
			ld bc, fs.fat_cache
			; l = LOW(cluster * 2)
			ld h, 0: add hl, hl
			; hl = (cluster * 4) % BYTES_PER_SECTOR
			add hl, bc: call __read_bcde

.error			pop hl
			ret

			; lseek(0)
			; hl -> FAT32_FILE
rewind			push hl, de
			call __rewind
			pop de, hl
			ret

			; reset file cluster to root dir
			; hl -> FAT32_FILE
__chroot		; get root directory LBA address
			ld bc, (fs.rootdir_cluster.hi)
			ld de, (fs.rootdir_cluster.lo)
			; open file by its cluster number
			; hl -> FAT32_FILE bcde = cluster
open_by_cluster		push hl
			call __open_by_cluster
			pop hl
			ret

			; lseek(0)
			; hl -> FAT32_FILE
__rewind		push hl
			.5 inc hl		; skip sr, lba
			call __read_bcde
			jr __store_current

			; hl -> FAT32_FILE bcde = cluster
__open_by_cluster	push hl
			.5 inc hl		; skip sr, lba
 			call __store_bcde	; store first_cluster
__store_current		call __store_bcde	; store current_cluster
			pop hl

			ld a, (fs.sectors_per_cluster): inc a
__store_alba		ld (hl), a: inc hl	; store sectors_read
			call get_cluster_lba
			jr __store_bcde

__dwcpy			push de
			call __read_bcde
			pop hl
__store_bcde		ld (hl), e: inc hl
			ld (hl), d: inc hl
__store_bc		ld (hl), c: inc hl
			ld (hl), b: inc hl
			ret

__read_bcde		ld e, (hl): inc hl
			ld d, (hl): inc hl
__read_bc		ld c, (hl): inc hl
			ld b, (hl): inc hl
			ret

__eq_bcde		ld a, (hl): cp e: ret nz: inc hl
			ld a, (hl): cp d: ret nz: inc hl
			ld a, (hl): cp c: ret nz: inc hl
			ld a, (hl): cp b: ret

			; hl -> FAT32_FILE
			; de -> destination
			; out: zf reset on error, cf set on end of cluster
fread512		push hl
			push de

			dec (hl): jr z, .next_cluster
			ld a, (fs.sectors_per_cluster)
			sub (hl): inc hl
			call __read_bcde ; cluster start_lba

			add e: ld e, a: jr nc, .read
			inc d: jr nz, .read: inc bc

.read			pop hl
			push hl
			call __read_sector
			pop de
			pop hl
			ret

.next_cluster		push hl
			ld bc, 9: add hl, bc
			push hl
			call __read_bcde	; read current_cluster
 			call get_next_cluster
			pop hl
		 	call __store_bcde	; store current_cluster
			pop hl

			ld a, b: cp $0f: jr nc, .end_of_chain ; TODO

			ld a, (fs.sectors_per_cluster)
			call __store_alba
			jr .read

.end_of_chain		pop de
			pop hl
			scf
			ret

			; get next FAT directory entry
			; in:	hl -> FAT32_DIR
			; out:	hl -> FAT32_DIR unchanged
			;	de -> FAT32_DIRENT
			;	__fn_buffer[] = file name
			; cf set if no more entries present
getent			push hl

			ex de, hl
			ld hl, (fs.rd_ptr) ; TODO
			xor a: ld (__fn_buffer), a

.loop			ld a, (fs.rd_entries_left) ; TODO
			or a: jr nz, .in_buffer

.read_fat		; read sector from FAT
			; one sector holds exactly 16 records, and no directory record
			; will ever cross a sector boundary
			ex de, hl
			ld de, fs.buffer: call fread512: jr nz, .error
			jr c,.error
			ex de, hl

			ld a, (BYTES_PER_SECTOR / DIRENT_SIZE) - 1
			ld (fs.rd_entries_left), a

			jr .parse_dirent

.end_of_directory	ld hl, 0: ld (fs.rd_ptr), hl
			xor a: ld (fs.rd_entries_left), a

.error			pop hl: scf: ret

.in_buffer		dec a
			ld (fs.rd_entries_left), a

.parse_dirent		ld a, (hl): or a: jr z, .end_of_directory ; no more directory entries
			cp $e5: jr z, .next ; unused entry

			push hl: ld bc, 11: add hl, bc: ld a, (hl): ld ixl, a: pop hl
			and FA_LFN: cp FA_LFN: jr z, .is_lfn_part
			and FA_VOLUMEID: jr nz, .next

			; lfn present?
			ld a, (__fn_buffer)
			or a: jr nz, .copy_attrs

.is_sfn			push de, hl
			ld de, __fn_buffer
			call __copy_sfn
			pop hl, de

.copy_attrs		; fatGetFirstCluster = ((readWord(dirPtr+20)<<16) + readWord(dirPtr+26))
			push hl
			ld bc, DIRENT_SIZE: add hl, bc: ld (fs.rd_ptr), hl
			pop de

			pop hl
			or a: ret

.is_lfn_part		push de
			push hl
			ld a, (hl): inc hl ; sequence number
			and $1f: dec a: ld e, a
			.2 add a: ld d, a: add a: add d: add e ; x13
			add LOW __fn_buffer: ld e, a
			adc HIGH __fn_buffer: sub e: ld d, a
			call __copy_lfn_part
			pop hl

			; last LFN part (comes first)?
			ld a, (hl): and $40: jr z, .next_pde ; no

			IFNDEF FAT32_ENABLE_CP866
.strip_ff_loop		dec de: ld a, (de)
			inc a: jr z, .strip_ff_loop
			xor a: inc de
			ELSE
			xor a
			ENDIF

			ld (de), a
.next_pde		pop de

.next			ld bc, DIRENT_SIZE: add hl, bc
			jp .loop


__copy_sfn		ld b, 8: call .copy
			ld a, (hl): cp $20: jr z, .end
			ld a, '.': ld (de), a: inc de
			ld b, 3: call .copy
.end			xor a: ld (de), a
			ret

.copy			ld a, (hl)
			cp $20: jr z, .next
			cp 'A': jr c, .store
			cp 'Z'+1: jr nc, .store
			add $20
.store			ld (de), a: inc de
.next			inc hl
			djnz .copy
			ret

__copy_lfn_part		; ASCII:	$0020..$007e -> $20..$7e
			; CYR:		$0401 (Ё), $0410..$044f (А-Яа-я), $0451 (ё)
			; CP866:	$80-$af (А-п), $e0-$ef (р-я), $f0 (Ё), $f1 (ё)

			IFNDEF FAT32_ENABLE_CP866

			ld bc, 5: call .loop: .3 inc hl
			ld c, 6: call .loop: .2 inc hl
			ld c, 2
.loop			ldi: inc hl: jp pe, .loop
			ret

			ELSE

			ld b, 5: call .loop: ret z
			.3 inc hl
			ld b, 6: call .loop: ret z
			.2 inc hl
			ld b, 2

.loop			ld c, (hl): inc hl
			ld a, (hl): inc hl
			or a: jr nz, .non_ascii
			bit 7, c: jr nz, .non_ascii
			or c: ret z
.store			ld (de), a: inc de
			djnz .loop
			inc b: ret

.non_ascii		call translate_ucs2: jr z, .store
			ld a, '?': jr .store

translate_ucs2		cp $04: ret nz: ld a, c
			cp $10: jr c, .lt_10
			cp $50: jr nc, .gt_4f
			cp $40: jr c, .lt_40		; А-п: $10..$3f -> $80..$af
			add $30				; р-я: $40..$4f -> $e0..$ef
.lt_40			add $70: cp a: ret		; $80..$af
.lt_10			cp $01: ret nz: ld a, $f0: ret	; capital_yo
.gt_4f			cp $51: ret nz: ld a, $f1: ret	; small_yo

			ENDIF


			; open dir/file by name (relative, hl->curdir)
			; hl -> FAT32_FILE
			; de -> filename.ext
			; out: cf set = file not found; FAT32_FILE updated
__open_by_name		xor a: ld (fs.rd_entries_left), a

			call rewind

			push de ; save fn ptr
			exx
			ld hl, __fn_buffer
			pop de
			exx

.loop			call getent: ret c
			exx: call stricmp: exx
			jr nz, .loop

.match			push hl

			; hl -> file.fsize
			ld bc, FAT32_FILE.fsize: add hl, bc

			ex de, hl
			ld c, FAT32_DIRENT.DIR_FstClusLO: add hl, bc
			call __read_bc: push bc

			; hl -> dirent.fsize
			; de -> file.fsize
			push hl: ld bc, 4: ldir: pop hl ; copy file size
			; TODO: set fpos to 0?

			ld bc, -8: add hl, bc ; hl -> FAT32_DIRENT.DIR_FstClusHI
			call __read_bc: pop de ; bcde = first cluster

			pop hl

			xor a: ld (fs.rd_entries_left), a
			call __open_by_cluster
			xor a: ret


			; open dir/file by slash-separated path name
			; hl -> FAT32_FILE
			; de -> path/filename.ext
			; cf set on error
__open_by_path		ld a, (de): cp '/': jr nz, .no_chroot
			push de: call __chroot: pop de: inc de

.no_chroot		ex de, hl ; de -> file
.loop			push hl
			ld c, '/': call strchr
			jr z, .dirname ; slash found

.filename		pop hl
			ex de, hl
			jp __open_by_name

.dirname		ld b, h: ld c, l
			ld a, (hl): ld (hl), 0
			pop hl

			ex de, hl
			; de -> start bc -> '/' hl -> file
			push bc
			push hl
			call __open_by_name
			pop de
			pop hl
			; hl -> '/'  de -> file
			ld (hl), '/': inc hl
			jr nc, .loop
			ret


chroot			equ __chroot
fopen			equ __open_by_path
chdir			equ __open_by_name
opendir			equ fopen
readdir			equ getent


			display /d, "FAT32_FS struct size: ", FAT32_FS, "b"
			display /d, "FAT32_FILE struct size: ", FAT32_FILE, "b"
			display /d, "FAT32_DIR struct size: ", FAT32_DIR, "b"

			display code, "-", $, ": FAT32 code (", /d, $-code, "b)"
			display fs, "-", fs + FAT32_FS, ": FAT32_FS struct (", /d, FAT32_FS, "b)"

			endmodule
			endif
