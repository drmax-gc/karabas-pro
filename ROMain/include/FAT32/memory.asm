			ifndef __MEMORY_ASM__
			define __MEMORY_ASM__

__memsetz_cpu		ld (hl), 0
			jr __memset_cpu.hl
__memset_cpu		ld (hl), a
.hl			dec bc
			ld d, h: ld e, l
			inc de
__memcpy_cpu		push af
			xor a: sub c: and 31: add a
			ld ($+4), a: jr nz, $
.loop			.32 ldi: jp pe, .loop
			pop af
			ret

memset			equ __memset_cpu
memsetz			equ __memsetz_cpu
memcpy			equ __memcpy_cpu

			macro _memsetz addr?, sz?
			ld hl, addr?: ld bc, sz?: call memsetz
			endm

			macro _memset addr?, val?, sz?
			ld hl, addr?: ld (hl), val?: ld bc, sz?: call __memset_cpu.hl
			endm

			macro _memcpy src?, dst?, sz?
			ld hl, src?: ld de, dst?: ld bc, sz?: call __memcpy_cpu
			endm


; -----------------------------------------------------------------------------
; Compare two ASCIIZ strings
; Input: HL, DE => strings to compare
; Output:
;	* Z is set if string heads match, else:
;	* C is set if HL > DE, reset otherwise
;
strcmp			IFUSED
			push hl, de
.loop			ld a, (de): or a: jr z, .leave
			cp (hl): inc hl: inc de
			jp z, .loop
.leave			pop de, hl
			ret
                        ENDIF

stricmp			IFUSED
			push hl, bc, de
			ld b, %100000
.loop			ld a, (hl): or b
			ld c, a
			ld a, (de): or b
			cp c: jr nz, .leave
			inc hl: inc de
			cp b: jp nz, .loop
.leave			pop de, bc, hl
			ret
			ENDIF

; -----------------------------------------------------------------------------
; Copy ASCIIZ string
; Input: HL => source, DE => dest
;
strcpy			IFUSED
			xor a
.loop			cp (hl): ldi: jp nz, .loop
			ret
			ENDIF

; -----------------------------------------------------------------------------
; Copy ASCIIZ string (up to BC-1 chars + zero)
; Input: HL => source, DE => dest, BC = max count
;
strlcpy			IFUSED
			xor a
.loop			cp (hl): ldi: ret z
			jp pe, .loop
			dec de: ld (de), a: inc de
			ret
			ENDIF

; -----------------------------------------------------------------------------
; Get ASCIIZ string length
; Input: HL => string
; Output: HL = string length
;
strlen			IFUSED
			xor a: ld b, a: ld c, a
			cpir: ld hl, $ffff: sbc hl, bc
			ret
			ENDIF

strchr			IFUSED
.loop			ld a, (hl): cp c: ret z
			inc hl: or a: jp nz, .loop
			dec hl: dec a: ret
			ENDIF

			endif
