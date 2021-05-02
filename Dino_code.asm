[org 0x100]
jmp start
bxer:dw 78
bxer2:dw 48
oldisr: dd 0
toldisr: dd 0
timebit: dw 0
score: dw 0
string1: db 'SCORE: '

printend:
pusha
 
push 0xb800
pop es
mov ah,01010111b
mov di,160*13+50

mov al,'G'
mov word [es:di + 2],ax
mov al,'A'
mov word [es:di + 4],ax
mov al,'M'
mov word [es:di + 6],ax
mov al,'E'
mov word [es:di + 8],ax
mov al,' '
mov word [es:di + 10],ax

mov al,'O'
mov word [es:di+12],ax
mov al,'V'
mov word [es:di+14],ax
mov al,'E'
mov word [es:di+16],ax
mov al,'R'
mov word [es:di+18],ax

mov ah,0Ah
mov al,'S'
mov word [es:di + 22],ax
mov al,'C'
mov word [es:di + 24],ax
mov al,'O'
mov word [es:di + 26],ax
mov al,'R'
mov word [es:di + 28],ax
mov al,'E'
mov word [es:di + 30],ax
mov al,':'
mov word [es:di + 32],ax

add di, 34
push word [score]
push di
call printnum

popa
ret

printstart:
pusha
 
push 0xb800
pop es
mov ah,01010111b
mov di,160*13+50

mov al,'P'
mov word [es:di],ax
mov al,'R'
mov word [es:di+2],ax
mov al,'E'
mov word [es:di+4],ax
mov al,'S'
mov word [es:di+6],ax
mov al,'S'
mov word [es:di+8],ax
mov al,' '
mov word [es:di+10],ax

mov al,'E'
mov word [es:di+12],ax
mov al,'N'
mov word [es:di+14],ax
mov al,'T'
mov word [es:di+16],ax
mov al,'E'
mov word [es:di+18],ax
mov al,'R'
mov word [es:di+20],ax
mov al,' '
mov word [es:di+22],ax


mov al,'T'
mov word [es:di+24],ax
mov al,'O'
mov word [es:di+26],ax
mov al,' '
mov word [es:di+28],ax
mov al,'S'
mov word [es:di+30],ax
mov al,'T'
mov word [es:di+32],ax
mov al,'A'
mov word [es:di+34],ax
mov al,'R'
mov word [es:di+36],ax
mov al,'T'
mov word [es:di+38],ax


popa
ret
printscore:
pusha
 
push 0xb800
pop es
mov ah,0Ah
mov al,'S'
mov word [es:130],ax
mov al,'C'
mov word [es:132],ax
mov al,'O'
mov word [es:134],ax
mov al,'R'
mov word [es:136],ax
mov al,'E'
mov word [es:138],ax
mov al,':'
mov word [es:140],ax
add word [score], 10
;my socre reser////////////////////////////////////
popa
ret

delayyy:
call printend

push cx
push ax
mov cx,10
;Jmp far [cs:oldisr]
lko:
call delay
;mov ah,0
;int 16h
;mov ah,28
;je startagain
loop lko
;mov ax, 2000
;lup1:
;mov cx, 3000
;lup:
;loop lup
;dec ax
;cmp ax, 0
;jne lup1
;jmp resisr
startagain:
pop ax
pop cx
mov word [cs:bxer],78
mov word [cs:bxer2],48
mov word [cs:score],0
jmp delayyyback


printstr:     push bp  
             mov bp, sp       

			 push es          
			 push ax            
			 push bx          
			 push cx          
			 push dx          

			 push si           
			 push di 
 
              mov  ax, 0xb800    
			  mov  es, ax            
			  ; point es to video base 
 
              mov  di, 80       
			  ; load di with columns per row  
			  mov  ax, [bp+10]        ; load ax with row number       
			  mul  di                 ; multiply with columns per row      
			  mov  di, ax             ; save result in di      
			  add  di, [bp+8]         ; add column number       
			  shl  di, 1              ; turn into byte count 
			  
 
              mov  si, [bp+6]         ; string to be printed       
			  mov  cx, [bp+4]         ; length of string           
			  mov  ah, 0x07           ; normal attribute is fixed 
 
 
nextchar:     mov  al, [si]           ; load next char of string    
           mov  [es:di], ax        ; show next char on screen        
		   add  di, 2              ; move to next screen location    
           add  si, 1              ; move to next char              
		   loop nextchar           ; repeat the operation cx times 
 
              pop  di       
			  pop  si       

			  pop  dx          

			  pop  cx            
			  pop  bx            
			  pop  ax 

  pop  es       
  pop  bp        
  ret  8 
 

printnum:
push bp
mov bp,sp
push es
push ax
push bx
push cx
push dx
push di

mov ax,0xb800
mov es,ax
mov ax,[bp+6]
mov bx,10
mov cx,0

nextdigit:
mov dx,0
div bx
add dl,0x30
push dx
inc cx
cmp ax,0
jnz nextdigit
;mov di,0
mov di,[bp+4]
nextpos:
pop dx
mov dh,0x07
mov [es:di],dx
add di,2
loop nextpos

pop di
pop dx
pop cx
pop bx
pop ax
pop es
pop bp
ret 4


clearscreen:
push ax
push di
push es
push 18
call dinosaur

mov ax,0xb800
mov es,ax
mov di,0
nextchar1:
mov word [es:di],0x0720
add di,2
cmp di, 4000
jne nextchar1
pop es
pop di
pop ax

ret

resisr:
xor ax, ax
mov es, ax
;jmp far[oldisr]
;jmp far[toldisr]
cli
push word [toldisr]
pop word [es: 8*4]
push word [toldisr + 2]
pop word [es: 8*4 + 2]
sti
cli
push word [oldisr]
pop word [es: 9*4]
push word [oldisr + 2]
pop word [es: 9*4 + 2]
sti
mov ax, 0x4c00
int 21h

dinoclearscreen:
push ax
push di
push es
push cx
mov ax,0xb800
mov es,ax
mov di,0
mov cx,23
klop1:
mov word [es:di],0x0720
add di,2
cmp di,26
jne klop1

add di,160
sub di,24
dec cx
cmp cx,0
jne klop1


pop cx
pop es
pop di
pop ax

ret

hurdle:
push bp
mov bp,sp
pusha
;save register
mov ax,[bp+4]; get index of hurdle(get column)
shl ax,1

mov di,160*19
add di,ax

mov cx,3

mov ah,00100000b
mov al,' '
push 0xb800
pop es
loop1:
cmp word [es:di - 6], 0x4020
je delayyy
delayyyback:
mov word [es:di],ax
add di,160
loop loop1
sub di,160



sub di,162
mov word [es:di],ax
sub di,162
mov word [es:di],ax

;add di,166
;mov word [es:di],ax

;sub di,160
;add di,2
;mov word [es:di],ax

popa
pop bp
ret 2
dinosaurjump:
push bp
mov bp,sp
;pusha
push cx
push bx

mov cx,4
mov bx,18
L5:

;call clearscreen
;call printline
push bx
call clearscreen
call dinosaur
;call delay
cmp cx,1
je next1
sub word [bxer],6
;call clearscreen
;call printline
next1:
sub bx,2
loop L5
mov cx,4



cmp word [cs:bxer],10
jb hskip
sub word [cs:bxer],6
hskip:
cmp word [cs:bxer2],10
jb hskip2
sub word [cs:bxer2],6
hskip2:
push word [cs:bxer]
call hurdle
push word [cs:bxer2]
call hurdle









call printscore


push word [cs:score]
push 150
call printnum
add word [cs:score],10



call delay
call delay
call delay

;call clearscreen



cmp word [cs:bxer],6
ja skipf

mov word [cs:bxer],78

skipf:
cmp word [cs:bxer2],6
ja skipf2

mov word [cs:bxer2],78
skipf2:

push word [cs:bxer]
call hurdle
push word [cs:bxer2]
call hurdle

call delay



add bx,2
L6:

;call clearscreen
;call printline
push bx
call clearscreen

call dinosaur
;call delay
cmp cx,1
je next
sub word [bxer],6

;call printline
next:
add bx,2
loop L6

pop bx
pop cx
pop bp

ret





dinosaur:
;save reg
;call clearscreen
push bp
mov bp,sp
pusha

push es

push 0xb800
pop es
mov bx,[bp+4]
mov cx,3

mov al,160
mul bl
add ax,8


mov di,ax
;loop will print straight line (body of dino)
;L1:
;mov ah, 0x07
;mov al,'*'
;mov word [es:di],ax
;add di,160
;loop L1

mov ah,01000000b
mov al,' '
;mov word [es:di],ax
add di,2
mov al,' '
mov word [es:di],ax
add di,2
mov al,'*'
mov word [es:di],ax
mov al,' '
sub di,2
add di,160
mov word [es:di],ax
sub di,2
mov word [es:di],ax
sub di,2
mov word [es:di],ax

add di,160
mov word [es:di],ax
add di,2
mov word [es:di],ax
add di,2
mov word [es:di],ax

add di,160
mov ah,07h
mov al,'|'
mov word [es:di],ax

sub di,4
mov word [es:di],ax

;print left leg
;sub di,2
;mov word [es:di],ax
;print right leg
;add di,4
;mov word [es:di],ax
;

;print right hand
;add di,2
;sub di,2*160
;mov word [es:di],ax
;print left hand
;sub di,8
;mov word [es:di],ax
pop es
popa
pop bp
ret 2
delay:
pusha
mov cx,100
mov ax,0
L2:

L3:
add ax,1
cmp ax,1000
jne L3
mov ax,0
loop L2
popa
ret

; keyboard interrupt service routine
tisr:

push di
push 0xb800
pop es


push word [cs:bxer]
call hurdle
push word [cs:bxer2]
call hurdle
push 18
call dinosaur
inc word [cs:timebit]
cmp word [cs:timebit],2
jne exit1122
mov word [cs:timebit],0



call printscore
push word [cs:score]
push 150
call printnum
add word [cs:score],10
push word [cs:bxer]
call hurdle



sub word [cs:bxer],6
cmp word [cs:bxer],0
ja skip12
mov  word [cs:bxer],78
skip12:
push word [cs:bxer2]
call hurdle


call delay



sub word [cs:bxer2],6
cmp word [cs:bxer2],0
ja skip1
mov  word [cs:bxer2],78
skip1:



call delay
;call delay
;call delay
;call delay
call delay
call delay

call clearscreen
exit1122:

;jmp far [cs:toldisr]

mov  al, 0x20  
             out  0x20, al
			 pop di
iret
 kbisr:      
  push ax       

        push es 

 
              mov  ax, 0xb800    
           mov  es, ax        
     ; point es to video memory 

 
              in   al, 0x60        
   ; read a char from keyboard port     
          cmp  al, 57    
       ; has the left shift pressed            
   jne  nextcmp        
    ; no, try next comparison 
 
              mov  byte [es:0], 'L' 
  ; yes, print L at first column  
call dinosaurjump  
      jmp  exit             
  ; leave interrupt routine 
 
nextcmp:      cmp  al, 0x36           ; has the right shift pressed           
    jne  nextcmp2           ; no, try next comparison 
 
              mov  byte [es:0], 'R'   ; yes, print R at second column         
      jmp  exit               ; leave interrupt routine 
 
nextcmp2:     cmp  al, 0xaa           ; has the left shift released         
      jne  nextcmp3           ; no, try next comparison 
 
              mov  byte [es:0], ' '   ; yes, clear the first column         
      jmp  exit               ; leave interrupt routine 
 
nextcmp3:     cmp  al, 0xb6           ; has the right shift released         
      jne  nomatch            ; no, chain to old ISR 
 
              mov  byte [es:2], ' '   ; yes, clear the second column         
      jmp  exit               ; leave interrupt routine 
 
nomatch:      pop  es     
          pop  ax            
   jmp  far [cs:oldisr]    ; call the original ISR 
 
exit:         
mov  al, 0x20  
out  0x20, al           ; send EOI to PIC 
pop  es           
pop  ax            
iret                    ; return from interrupt 
 
start:

call printstart
mov ah,0
int 16h
cmp ah,28
jne terminate
xor  ax, ax     
mov  es, ax             ; point es to IVT base      
mov ax, [es: 8*4]
mov [toldisr], ax
mov ax, [es: 8*4 + 2]
mov [toldisr + 2], ax
mov  ax, [es:9*4]            
   mov  [oldisr], ax       ; save offset of old routine            
   mov  ax, [es:9*4+2]         
      mov  [oldisr+2], ax     ; save segment of old routine
	  
        cli                     ; disable interrupts          
     mov  word [es:9*4], kbisr ; store offset at n*4         
      mov  [es:9*4+2], cs     ; store segment at n*4+2          
     sti                     ; enable interrupts 


   cli                     ; disable interrupts          
     mov  word [es:8*4], tisr ; store offset at n*4         
      mov  [es:8*4+2], cs     ; store segment at n*4+2          
     sti       

  terminate:
			mov  dx, start          ; end of resident portion       
        add  dx, 15             ; round up to next para           
    mov  cl, 4          
     shr  dx, cl             ; number of paras        
        mov  ax, 0x3100         ; terminate and stay resident             
  int  0x21