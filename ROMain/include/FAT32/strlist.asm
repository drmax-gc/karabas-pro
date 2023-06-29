			ifndef __STRLIST_ASM__
			define __STRLIST_ASM__

			include "memory.asm"

			module strlist

; Упорядоченный список строк с сортировкой при вставке.
; Индекс размещается с $c000 (513 байт), сами строки - сразу после индекса и до конца страницы.
; После 255 строк (или если вставляемая строка уже не вмещается в остаток страницы) новые вставки игнорятся.

; init, clear: очистить список строк
; add: добавить в список строку по адресу HL
; get: получить в HL адрес строки с порядковым номером А (A = 0..254)

			struct STRLIST
			align 256
index
lo			ds 255, 0
count			db 0
hi			ds 255, 0
next			dw 0
			ends

strlist			STRLIST = $8000

count			equ strlist.count

init
clear			_memsetz strlist, STRLIST-2
			ld hl, strlist + STRLIST
			ld (strlist.next), hl
			ret

; a = index
; out: hl -> strings[index]
;      cf = index > count
get			ld l, a
			ld a, (strlist.count)
			cp l: ret c: ret z
			ld h, HIGH strlist.index: ld e, (hl)
			inc h: ld d, (hl)
			ex de, hl
			ret

; hl -> string to add
; out: cf  = out of ram
;      zf  = too many strings
add			ld a, (strlist.count)
			inc a: ret z

			exa

			push hl
			call strlen
			ld de, (strlist.next)
			add hl, de
			pop hl
			ret c ; out of ram

			push de
			call strcpy
			ld (strlist.next), de
			pop de

			exa
			ld (strlist.count), a

			; update index
			dec a: jr z, .insert

			dec a: ld c, a: xor a ; a = low = 0, c = high = (count) - 2
			push bc ; keep c

			push de
			exx
			pop de
			ld b, HIGH strlist.index
			exx

			call __bsearch ; a = insertion index
			pop bc

			ld b, a
			ld a, c: sub b: inc a: ld c, a
			ld a, b ; c = count to move; a = index
			jr z, .insert
			jr c, .insert

			; move [a..c] to [a+1..c+1]
			push de, af
			ld b, 0
			ld h, HIGH strlist.index: ld d, h
			ld a, (strlist.count): dec a: ld l, a: ld e, a: dec l
			push hl, de, bc
			lddr
			pop bc, de, hl: inc h: inc d
			lddr
			pop af, de

.insert			ld h, HIGH strlist.index: ld l, a
			ld (hl), e: inc h: ld (hl), d
			dec b: ret


			; 8 bit binary search
			; https://en.wikipedia.org/wiki/Binary_search_algorithm
			; a = L; c = R
__bsearch		dec a
.loop_lt		inc a
			ld b, a ; b = L

			inc c
.loop_gt		cp c: ret nc ; done if b:L > c:R
			dec c

			add c: rra ; a = m := floor((L + R) // 2)

			; cf set if DE:T > HL:A[m]
.compare		exx
			ld c, a
			ld a, (bc): ld l, a: inc b
			ld a, (bc): ld h, a: dec b
			call stricmp
			ld a, c
			exx

			; Z: return m
			ret z

			; NC: A[m] < T: b:L := a:m + 1
			jp nc, .loop_lt

			; C: A[m] > T: c:R := a:m - 1
			ld c, a: ld a, b: jp .loop_gt

			endmodule
			endif
