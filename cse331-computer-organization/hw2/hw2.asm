.data
inpArray: .word 3,10,7,9,4,11 # Input array
arraySize: .word 6 # Size of the input array
lis: .space 128 # n*n*4 = 6*6*4 = 128, 2d array size allocated
lisz: .space 24 # lis size of each sequence in lis array
newline: .asciiz "\n"

.text
main:
la $s0, inpArray # $s0 = Address of the input array

findLIS:
lw $s1, arraySize # $s1 = Size of the input array
beq $s1, $zero, exit # Base case, size = 0 ?

la $s2, lis # $s2 = Address of 2d lis array
la $s3, lisz # $s3 = Address of lisz array

lw $t0, ($s0) # $t0 = first element of the input array
sw $t0, ($s2) # lis[0] = longest subsequence ending at array[0]

li $t0, 1
sw $t0, ($s3) # lisz[0] = 1

la $a0, ($s2)
lw $a1, ($s3)
jal printArray

######## START OF FIRST NESTED LOOP ########
li $t1, 1 # $t1 = i = 1
forOut:
beq $t1, $s1, printAllLisArrays

li $t2, 0 # %t2 = j = 0
forIn:
beq $t2, $t1, increment # j == i

# Calculate arr[i]
sll $t4, $t1, 2
add $t5, $s0, $t4
lw $t6, ($t5) # $t6 = arr[i]

# Calculate arr[j]
sll $t4, $t2, 2
add $t5, $s0, $t4
lw $t7, ($t5) # $t7 = arr[j]

bge $t7, $t6, include

# Calculate lisz[i]
sll $t4, $t1, 2
add $t5, $s3, $t4
lw $t6, ($t5) # $t6 = lisz[i]

# Calculate lisz[j]
sll $t4, $t2, 2
add $t5, $s3, $t4
lw $t7, ($t5) # $t7 = lisz[j]

ble $t7, $t6, include

# Calculate lis[i]
sll $t4, $t1, 2
mul $t4, $t4, 6
add $t5, $s2, $t4 # $t5 = address of lis[i]

# Calculate lis[j]
sll $t4, $t2, 2
mul $t4, $t4, 6
add $t6, $s2, $t4 # $t6 = address of lis[j]

# lis[i] = lis[j]
moveContents: # Move the contents of lis[j] to lis[i]
la $s7, ($t1)
li $t1, 0 # k = 0
mcloop: # k = 0 to lisz[j]
beq $t1, $t7, include # k == lisz[j] ?
sll $t2, $t1, 2
add $t2, $t2, $t5 
lw $t3, ($t2) # $t3 = lis[i][k] 
beq $t3, $zero, addLisJ
j moveContents

addLisJ: # lis[i][k] = lis[j][k]
sll $t2, $t1, 2
add $t2, $t2, $t6
lw $t4, ($t2) # $t4 = lis[j][k]
addi $t1, $t1, 1
j mcloop

include:
# Find lis[i]
la $t1, ($s7) 
sll $t3, $t1, 2 # Multiply i with 4
mult $t3, $s1 # Multiply the result with arraysize
mflo $t4 # Result of mult
add $t4, $s2, $t4 # $t4 = Address of lis[i]

# Find empty space and include arr[i]
li $t2, 0 # j = 0

findEmptySpaceInclude:# find lis[j]=0 
sll $t6, $t2, 2
add $t7, $t4, $t6 # $t7 = address of lis[j]
lw $t5, ($t7) # $t5 = lis[j]
beq $t5, $zero, includeElement
addi $t2, $t2, 1
j findEmptySpaceInclude

includeElement:
la $t1, ($s7)
sll $t3, $t1, 2 # Multiply i with 4
add $t6, $t3, $s0 # $t6 = address of arr[i]
lw $t6, ($t6)
sw $t6, ($t7) 

increment:
addi, $t1, $t1, 1
j forOut
######## END OF FIRST NESTED LOOP ########

li $t0, 0 # i = 0
printAllLisArrays:
# find lis[i]
sll $t2, $t0, 2 # i*=4
mult $t2, $s1 # (4i)*n
mflo $t3
add $t4, $t3, $s2 # $t4 = address of lis[i]
# find lisz[i]
add $t5, $t2, $s3 # $t5 = addres of lisz[i]
lw $a0, ($t4)
lw $a1, ($t5)
jal printArray

addi $t2, $t1, 1
j printAllLisArrays

printArray:
la $t1, ($a0) # $t1 = $a0 = address of the array to be printed
la $t2, ($a1) # $t2 = $a1 = size of the array to be printed

li $t0, 0 # $t0 = i = 0
printLoop:
beq $t0, $t2, printnl # i < n ?
sll $t4, $t0, 2 # mult by 4 to find next int
add $t3, $t4, $t1 # $t3 = arr[i]

# print arr[i]
li $v0, 1
la $a0, ($t3) 
syscall

addi $t0, $t0, 1
j printLoop

printnl:
li $v0, 4
la $a0, newline
syscall

goback:
jr $ra

exit: # Terminate the program
li $v0, 10
syscall




