#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>

/*
For the language grammar, please refer to Grammar section on the github page:
  https://github.com/lightbulb12294/CSI2P-II-Mini1#grammar
*/

#define MAX_LENGTH 200
typedef enum {
	ASSIGN, ADD, SUB, MUL, DIV, REM, PREINC, PREDEC, POSTINC, POSTDEC, IDENTIFIER, CONSTANT, LPAR, RPAR, PLUS, MINUS, END
} Kind;
typedef enum {
	STMT, EXPR, ASSIGN_EXPR, ADD_EXPR, MUL_EXPR, UNARY_EXPR, POSTFIX_EXPR, PRI_EXPR
} GrammarState;
typedef struct TokenUnit {
	Kind kind;
	int val; // record the integer value or variable name
	struct TokenUnit *next;
} Token;
typedef struct ASTUnit {
	Kind kind;
	int val; // record the integer value or variable name
	struct ASTUnit *lhs, *mid, *rhs;
} AST;

/// utility interfaces

// err marco should be used when a expression error occurs.
#define err(x) {\
	puts("Compile Error!");\
	if(DEBUG) {\
		fprintf(stderr, "Error at line: %d\n", __LINE__);\
		fprintf(stderr, "Error message: %s\n", x);\
	}\
	exit(0);\
}
// You may set DEBUG=1 to debug. Remember setting back to 0 before submit.
#define DEBUG 0
// Split the input char array into token linked list.
Token *lexer(const char *in);
// Create a new token.
Token *new_token(Kind kind, int val);
// Translate a token linked list into array, return its length.
size_t token_list_to_arr(Token **head);
// Parse the token array. Return the constructed AST.
AST *parser(Token *arr, size_t len);
// Parse the token array. Return the constructed AST.
AST *parse(Token *arr, int l, int r, GrammarState S);
// Create a new AST node.
AST *new_AST(Kind kind, int val);
// Find the location of next token that fits the condition(cond). Return -1 if not found. Search direction from start to end.
int findNextSection(Token *arr, int start, int end, int (*cond)(Kind));
// Return 1 if kind is ASSIGN.
int condASSIGN(Kind kind);
// Return 1 if kind is ADD or SUB.
int condADD(Kind kind);
// Return 1 if kind is MUL, DIV, or REM.
int condMUL(Kind kind);
// Return 1 if kind is RPAR.
int condRPAR(Kind kind);
// Check if the AST is semantically right. This function will call err() automatically if check failed.
void semantic_check(AST *now);
// Generate ASM code.
void codegen(AST *root);
// Free the whole AST.
void freeAST(AST *now);

/// debug interfaces

// Print token array.
void token_print(Token *in, size_t len);
// Print AST tree.
void AST_print(AST *head);

char input[MAX_LENGTH];

int main() {
	while (fgets(input, MAX_LENGTH, stdin) != NULL) {
		Token *content = lexer(input);
		size_t len = token_list_to_arr(&content);
		if (len == 0) continue;
		AST *ast_root = parser(content, len);
		semantic_check(ast_root);

		if(DEBUG){
			AST_print(ast_root);
			token_print(content, len);
		}
		
		codegen(ast_root);
		free(content);
		freeAST(ast_root);
	}
	return 0;
}

Token *lexer(const char *in) {
	Token *head = NULL;
	Token **now = &head;
	for (int i = 0; in[i]; i++) {
		if (isspace(in[i])) // ignore space characters
			continue;
		else if (isdigit(in[i])) {
			(*now) = new_token(CONSTANT, atoi(in + i));
			while (in[i+1] && isdigit(in[i+1])) i++;
		}
		else if ('x' <= in[i] && in[i] <= 'z') // variable
			(*now) = new_token(IDENTIFIER, in[i]);
		else switch (in[i]) {
			case '=':
				(*now) = new_token(ASSIGN, 0);
				break;
			case '+':
				if (in[i+1] && in[i+1] == '+') {
					i++;
					// In lexer scope, all "++" will be labeled as PREINC.
					(*now) = new_token(PREINC, 0);
				}
				// In lexer scope, all single "+" will be labeled as PLUS.
				else (*now) = new_token(PLUS, 0);
				break;
			case '-':
				if (in[i+1] && in[i+1] == '-') {
					i++;
					// In lexer scope, all "--" will be labeled as PREDEC.
					(*now) = new_token(PREDEC, 0);
				}
				// In lexer scope, all single "-" will be labeled as MINUS.
				else (*now) = new_token(MINUS, 0);
				break;
			case '*':
				(*now) = new_token(MUL, 0);
				break;
			case '/':
				(*now) = new_token(DIV, 0);
				break;
			case '%':
				(*now) = new_token(REM, 0);
				break;
			case '(':
				(*now) = new_token(LPAR, 0);
				break;
			case ')':
				(*now) = new_token(RPAR, 0);
				break;
			case ';':
				(*now) = new_token(END, 0);
				break;
			default:
				err("Unexpected character.");
		}
		now = &((*now)->next);
	}
	return head;
}

Token *new_token(Kind kind, int val) {
	Token *res = (Token*)malloc(sizeof(Token));
	res->kind = kind;
	res->val = val;
	res->next = NULL;
	return res;
}

size_t token_list_to_arr(Token **head) {
	size_t res;
	Token *now = (*head), *del;
	for (res = 0; now != NULL; res++) now = now->next;
	now = (*head);
	if (res != 0) (*head) = (Token*)malloc(sizeof(Token) * res);
	for (int i = 0; i < res; i++) {
		(*head)[i] = (*now);
		del = now;
		now = now->next;
		free(del);
	}
	return res;
}

AST *parser(Token *arr, size_t len) {
	for (int i = 1; i < len; i++) {
		// correctly identify "ADD" and "SUB"
		if (arr[i].kind == PLUS || arr[i].kind == MINUS) {
			switch (arr[i - 1].kind) {
				case PREINC:
				case PREDEC:
				case IDENTIFIER:
				case CONSTANT:
				case RPAR:
					arr[i].kind = arr[i].kind - PLUS + ADD;
				default: break;
			}
		}
	}
	return parse(arr, 0, len - 1, STMT);
}

AST *parse(Token *arr, int l, int r, GrammarState S) {
	AST *now = NULL;
	if (l > r)
		err("Unexpected parsing range.");
	int nxt;
	switch (S) {
		case STMT:
			if (l == r && arr[l].kind == END)
				return NULL;
			else if (arr[r].kind == END)
				return parse(arr, l, r - 1, EXPR);
			else err("Expected \';\' at the end of line.");
		case EXPR:
			return parse(arr, l, r, ASSIGN_EXPR);
		case ASSIGN_EXPR:
			if ((nxt = findNextSection(arr, l, r, condASSIGN)) != -1) {
				now = new_AST(arr[nxt].kind, 0);
				now->lhs = parse(arr, l, nxt - 1, UNARY_EXPR);
				now->rhs = parse(arr, nxt + 1, r, ASSIGN_EXPR);
				return now;
			}
			return parse(arr, l, r, ADD_EXPR);
		case ADD_EXPR:
			if((nxt = findNextSection(arr, r, l, condADD)) != -1) {
				now = new_AST(arr[nxt].kind, 0);
				now->lhs = parse(arr, l, nxt - 1, ADD_EXPR);
				now->rhs = parse(arr, nxt + 1, r, MUL_EXPR);
				return now;
			}
			return parse(arr, l, r, MUL_EXPR);
		case MUL_EXPR:
			// TODO: Implement MUL_EXPR.
			// hint: Take ADD_EXPR as reference.
			if((nxt = findNextSection(arr, r, l, condMUL)) != -1) {
				now = new_AST(arr[nxt].kind, 0);
				now->lhs = parse(arr, l, nxt - 1, MUL_EXPR);
				now->rhs = parse(arr, nxt + 1, r, UNARY_EXPR);
				return now;
			}
			return parse(arr, l, r, UNARY_EXPR);
		case UNARY_EXPR:
			// TODO: Implement UNARY_EXPR.
			// hint: Take POSTFIX_EXPR as reference.
			if (arr[l].kind == PREINC || arr[l].kind == PREDEC || arr[l].kind == PLUS || arr[l].kind == MINUS){
				now = new_AST(arr[l].kind, 0);
				now->mid = parse(arr, l + 1, r, UNARY_EXPR);
				return now;
			}
			return parse(arr, l, r, POSTFIX_EXPR);
		case POSTFIX_EXPR:
			if (arr[r].kind == PREINC || arr[r].kind == PREDEC) {
				// translate "PREINC", "PREDEC" into "POSTINC", "POSTDEC"
				now = new_AST(arr[r].kind - PREINC + POSTINC, 0);
				now->mid = parse(arr, l, r - 1, POSTFIX_EXPR);
				return now;
			}
			return parse(arr, l, r, PRI_EXPR);
		case PRI_EXPR:
			if (findNextSection(arr, l, r, condRPAR) == r) {
				now = new_AST(LPAR, 0);
				now->mid = parse(arr, l + 1, r - 1, EXPR);
				return now;
			}
			if (l == r) {
				if (arr[l].kind == IDENTIFIER || arr[l].kind == CONSTANT)
					return new_AST(arr[l].kind, arr[l].val);
				err("Unexpected token during parsing.");
			}
			err("No token left for parsing.");
		default:
			err("Unexpected grammar state.");
	}
}

AST *new_AST(Kind kind, int val) {
	AST *res = (AST*)malloc(sizeof(AST));
	res->kind = kind;
	res->val = val;
	res->lhs = res->mid = res->rhs = NULL;
	return res;
}

int findNextSection(Token *arr, int start, int end, int (*cond)(Kind)) {
	int par = 0;
	int d = (start < end) ? 1 : -1;
	for (int i = start; (start < end) ? (i <= end) : (i >= end); i += d) {
		if (arr[i].kind == LPAR) par++;
		if (arr[i].kind == RPAR) par--;
		if (par == 0 && cond(arr[i].kind) == 1) return i;
	}
	return -1;
}

int condASSIGN(Kind kind) {
	return kind == ASSIGN;
}

int condADD(Kind kind) {
	return kind == ADD || kind == SUB;
}

int condMUL(Kind kind) {
	return kind == MUL || kind == DIV || kind == REM;
}

int condRPAR(Kind kind) {
	return kind == RPAR;
}

void semantic_check(AST *now) {
	if (now == NULL) return;
	// Left operand of '=' must be an identifier or identifier with one or more parentheses.
	if (now->kind == ASSIGN) {
		AST *tmp = now->lhs;
		while (tmp->kind == LPAR) tmp = tmp->mid;
		if (tmp->kind != IDENTIFIER)
			err("Lvalue is required as left operand of assignment.");
	}
	// Operand of INC/DEC must be an identifier or identifier with one or more parentheses.
	// TODO: Implement the remaining semantic_check code.
	// hint: Follow the instruction above and ASSIGN-part code to implement.
	// hint: Semantic of each node needs to be checked recursively (from the current node to lhs/mid/rhs node).
	if (now->kind == PREINC || now->kind == PREDEC) {
		AST *tmp = now->mid;
		while (tmp->kind == LPAR) tmp = tmp->mid;
		if (tmp->kind != IDENTIFIER)
			err("Rvalue is required as rigth operand of pre-increment.");
	}
	if (now->kind == POSTINC || now->kind == POSTDEC) {
		AST *tmp = now->mid;
		while (tmp->kind == LPAR) tmp = tmp->mid;
		if (tmp->kind != IDENTIFIER)
			err("Lvalue is required as left operand of post-increment.");
	}
	semantic_check(now->lhs);
	semantic_check(now->mid);
	semantic_check(now->rhs);
}

void custom_err(char *str){
	char msg[1000] = {0};
	sprintf(msg, "%s\n", str);
	perror(msg);	
}

typedef enum {
	OP_LOAD, OP_STORE, OP_ADD, OP_SUB, OP_MUL, OP_DIV, OP_REM
} OPCODE;

typedef enum{
	REG_TYPE, CONST_TYPE, MEM_TYPE
}SPACE_TYPE;

typedef struct{
	int addr;
}Memory;

typedef struct{
	int n;
}Register;

typedef struct{
	int c;
}Constant;

typedef struct space{
	SPACE_TYPE type;
	Register reg;
	Memory mem;
	Constant constant;
} Space;

typedef struct isa{
	OPCODE opcode;
	Space rd, rs1, rs2;
}ISA;

typedef struct isa_node{
	ISA isa;
	struct isa_node *prev, *next;
}ISA_Node;

int mem_addr(int n){
	return n * 4;
}

Memory mem(int addr){
	Memory x;
	x.addr = addr;
	return x;
}

Register reg(int n){
	Register x;
	x.n = n;
	return x;
}

Space reg2space(Register reg){
	Space sp;
	sp.type = REG_TYPE;
	sp.reg = reg;
	return sp;
}

Space mem2space(Memory mem){
	Space sp;
	sp.type = MEM_TYPE;
	sp.mem = mem;
	return sp;
}

Space const2space(Constant c){
	Space sp;
	sp.type = CONST_TYPE;
	sp.constant = c;
	return sp;
}

int get_stack_ptr_inc(int stack_ptr){
	return stack_ptr + 4;
}

int get_stack_ptr_dec(int stack_ptr){
	return stack_ptr - 4;
}

int stack_ptr_inc(int *stack_ptr){
	*stack_ptr = get_stack_ptr_inc(*stack_ptr);
	return *stack_ptr;
}

int stack_ptr_dec(int *stack_ptr){
	*stack_ptr = get_stack_ptr_dec(*stack_ptr);
	return *stack_ptr;
}

typedef enum{
	ASSIGN_MODE, COMPUTE_MODE
}CODE_GEN_MODE;

typedef enum{
	VAR_X, VAR_Y, VAR_Z
}VAR;

#define CUSTOM_ERR_LEN 100
#define OP_BUF_LEN 10
#define SPACE_BUF_LEN 10
#define ASM_BUF_LEN 100

#define RSV_RD_REG 0
#define RSV_RS1_REG 1
#define RSV_RS2_REG 2
#define IDEN_X_REG 3
#define IDEN_Y_REG 4
#define IDEN_Z_REG 5
#define IDEN_X_REG_W 6
#define IDEN_Y_REG_W 7
#define IDEN_Z_REG_W 8
#define IDEN_X 'x'
#define IDEN_Y 'y'
#define IDEN_Z 'z'
#define IDEN_X_MEM 0
#define IDEN_Y_MEM 4
#define IDEN_Z_MEM 8

Register iden2reg(int iden){
	switch (iden){
		case IDEN_X:
			return reg(IDEN_X_REG);
		case IDEN_Y:
			return reg(IDEN_Y_REG);
		case IDEN_Z:
			return reg(IDEN_Z_REG);
		default: ;
			char buf[CUSTOM_ERR_LEN] = {0};
			sprintf(buf, "Error: No such identifier called %d, unable to get register\n", iden);
			custom_err(buf);
			return reg(IDEN_X_REG);
	}
}

Register iden2reg_r(int iden){
	switch (iden){
		case IDEN_X:
			return reg(IDEN_X_REG);
		case IDEN_Y:
			return reg(IDEN_Y_REG);
		case IDEN_Z:
			return reg(IDEN_Z_REG);
		default: ;
			char buf[CUSTOM_ERR_LEN] = {0};
			sprintf(buf, "Error: No such identifier called %d, unable to get register\n", iden);
			custom_err(buf);
			return reg(IDEN_X_REG);
	}
}

Register iden2reg_w(int iden){
	switch (iden){
		case IDEN_X:
			return reg(IDEN_X_REG_W);
		case IDEN_Y:
			return reg(IDEN_Y_REG_W);
		case IDEN_Z:
			return reg(IDEN_Z_REG_W);
		default: ;
			char buf[CUSTOM_ERR_LEN] = {0};
			sprintf(buf, "Error: No such identifier called %d, unable to get register\n", iden);
			custom_err(buf);
			return reg(IDEN_X_REG_W);
	}
}

Memory iden2mem(int iden){
	switch (iden){
		case IDEN_X:
			return mem(IDEN_X_MEM);
		case IDEN_Y:
			return mem(IDEN_Y_MEM);
		case IDEN_Z:
			return mem(IDEN_Z_MEM);
		default: ;
			char buf[CUSTOM_ERR_LEN] = {0};
			sprintf(buf, "Error: No such identifier called %d, unable to get memory address\n", iden);
			custom_err(buf);
			return mem(IDEN_X_MEM);
	}
}

ISA_Node *head = NULL, *tail = NULL;

ISA get_isa(OPCODE opcode, Space rd, Space rs1, Space rs2){
	ISA isa;
	isa.opcode = opcode;
	isa.rd = rd;
	isa.rs1 = rs1;
	isa.rs2 = rs2;
	return isa;
}

void opcode2asm(char *buf, OPCODE opcode){
	switch (opcode){
	case OP_ADD:
		strcpy(buf, "add");
		return;
	case OP_SUB:
		strcpy(buf, "sub");
		return;
	case OP_MUL:
		strcpy(buf, "mul");
		return;
	case OP_DIV:
		strcpy(buf, "div");
		return;
	case OP_REM:
		strcpy(buf, "rem");
		return;
	case OP_LOAD:
		strcpy(buf, "load");
		return;
	case OP_STORE:
		strcpy(buf, "store");
		return;
	default: ;
		char buf[CUSTOM_ERR_LEN] = {0};
		sprintf(buf, "Error: No such opcode called %d, unable to convert it to asm code\n", opcode);
		custom_err(buf);
		return;
	}
}

void space2asm(char *space_buf, Space s){
	if(s.type == REG_TYPE){
		sprintf(space_buf, "r%d", s.reg.n);
	}else if(s.type == MEM_TYPE){
		sprintf(space_buf, "[%d]", s.mem.addr);
	}else if(s.type == CONST_TYPE){
		sprintf(space_buf, "%d", s.constant.c);
	}else{
		char buf[CUSTOM_ERR_LEN] = {0};
		sprintf(buf, "Error: Invalid space type: %d, unable to convert it to asm code\n", s.type);
		custom_err(buf);
	}
}

void isa2asm(char *asm_buf, ISA isa){
	char op_buf[OP_BUF_LEN] = {0};
	opcode2asm(op_buf, isa.opcode);

	if(isa.opcode == OP_LOAD){
		if(isa.rd.type == REG_TYPE && isa.rs1.type == MEM_TYPE){
			char rd_str[SPACE_BUF_LEN] = {0}, rs1_str[SPACE_BUF_LEN] = {0};
			space2asm(rd_str, isa.rd);
			space2asm(rs1_str, isa.rs1);

			sprintf(asm_buf, "%s %s %s\n", op_buf, rd_str, rs1_str);
		}else{
			char buf[CUSTOM_ERR_LEN] = {0};
			sprintf(buf, "Error: Invalid rd type: %d or rs type: %d for load, unable to convert it to asm code\n", isa.rd.type, isa.rs1.type);
			custom_err(buf);
		}
	}else if(isa.opcode == OP_STORE){
		if(isa.rd.type == MEM_TYPE && isa.rs1.type == REG_TYPE){
			char rd_str[SPACE_BUF_LEN] = {0}, rs1_str[SPACE_BUF_LEN] = {0};
			space2asm(rd_str, isa.rd);
			space2asm(rs1_str, isa.rs1);

			sprintf(asm_buf, "%s %s %s\n", op_buf, rd_str, rs1_str);
		}else{
			char buf[CUSTOM_ERR_LEN] = {0};
			sprintf(buf, "Error: Invalid rd type: %d or rs type: %d for store, unable to convert it to asm code\n", isa.rd.type, isa.rs1.type);
			custom_err(buf);
		}
	}else{
		if(isa.rd.type == REG_TYPE && (isa.rs1.type == CONST_TYPE || isa.rs1.type == REG_TYPE) && (isa.rs2.type == CONST_TYPE || isa.rs2.type == REG_TYPE)){
			char rd_str[SPACE_BUF_LEN] = {0}, rs1_str[SPACE_BUF_LEN] = {0}, rs2_str[SPACE_BUF_LEN] = {0};
			space2asm(rd_str, isa.rd);
			space2asm(rs1_str, isa.rs1);
			space2asm(rs2_str, isa.rs2);
			
			sprintf(asm_buf, "%s %s %s %s\n", op_buf, rd_str, rs1_str, rs2_str);
		}
	}
}

void init_asm(){
	if(head != NULL){
		free(head);
	}
	head = (ISA_Node*)malloc(sizeof(ISA_Node));
	head->next = head;
	head->prev = head;
	tail = head;
}

ISA_Node *insert_asm(ISA asm_code){
	if(head == NULL){
		init_asm();
	}
	ISA_Node *node = (ISA_Node*)malloc(sizeof(ISA_Node));
	node->isa = asm_code;
	node->prev = tail;
	node->next = tail->next;
	tail->next = node;
	tail->next->prev = node;
	tail = node;
	return node;
}

void set_reg(Register reg, int val){
	if(val >= 0){
		// printf("add r%d 0 %d\n", reg.n, val);
		Constant c1 = {0}, c2 = {val};
		ISA isa = {OP_ADD, reg2space(reg), const2space(c1), const2space(c2)};
		
		// c1.c = 0;
		// c2.c = val;
		// isa.opcode = OP_ADD;
		// isa.rd = reg2space(reg);
		// isa.rs1 = const2space(c1);
		// isa.rs2 = const2space(c2);
		
		char asm_buf[ASM_BUF_LEN] = {0};
		isa2asm(asm_buf, isa);
		printf("%s", asm_buf);
	}else{
		// printf("sub r%d 0 %d\n", reg.n, -val);
		Constant c1 = {0}, c2 = {-val};
		ISA isa = {OP_SUB, reg2space(reg), const2space(c1), const2space(c2)};
		// ISA isa;
		// Constant c1, c2;
		// c1.c = 0;
		// c2.c = val;
		// isa.opcode = OP_SUB;
		// isa.rd = reg2space(reg);
		// isa.rs1 = const2space(c1);
		// isa.rs2 = const2space(c2);
		
		char asm_buf[ASM_BUF_LEN] = {0};
		isa2asm(asm_buf, isa);
		printf("%s", asm_buf);
	}
}

ISA asm_store(Memory mem, Register reg){
	// printf("store [%d] r%d\n", mem.addr, reg.n);
	ISA isa;
	isa.opcode = OP_STORE;
	isa.rd = mem2space(mem);
	isa.rs1 = reg2space(reg);

	char asm_buf[ASM_BUF_LEN] = {0};
	isa2asm(asm_buf, isa);
	printf("%s", asm_buf);
	return isa;
}

ISA asm_load(Register reg, Memory mem){
	// printf("load r%d [%d]\n", reg.n, mem.addr);
	ISA isa;
	isa.opcode = OP_LOAD;
	isa.rd = reg2space(reg);
	isa.rs1 = mem2space(mem);

	char asm_buf[ASM_BUF_LEN] = {0};
	isa2asm(asm_buf, isa);
	printf("%s", asm_buf);
	return isa;
}

ISA asm_arithmetic(OPCODE opcode, Register rd, Space s1, Space s2){
	ISA isa = {opcode, reg2space(rd), s1, s2};
	// isa.opcode = opcode;
	// isa.rd = reg2space(rd);
	// isa.rs1 = s1;
	// isa.rs2 = s2;

	char asm_buf[ASM_BUF_LEN] = {0};
	isa2asm(asm_buf, isa);
	printf("%s", asm_buf);
	return isa;
}

void push(Register reg, int *stack_ptr){
	asm_store(mem(*stack_ptr), reg);
	// (*stack_ptr)+=4;
	stack_ptr_inc(stack_ptr);
}

void pop(Register reg, int *stack_ptr){
	// (*stack_ptr)-=4;
	stack_ptr_dec(stack_ptr);
	asm_load(reg, mem(*stack_ptr));
}

void assign(int iden, int *stack_ptr){
	// Register target_reg = iden2reg_w(iden);
	Register target_reg = iden2reg(iden);
	asm_load(target_reg, mem(get_stack_ptr_dec(*stack_ptr)));
}

// void assign_sp(int iden, int *stack_ptr){
// 	Register target_reg = iden2reg_w(iden);
// 	// asm_load(target_reg, mem((*stack_ptr) - 4));
// 	asm_load(target_reg, mem(get_stack_ptr_dec(*stack_ptr)));
// }

void arithmetic(OPCODE opcode, int *stack_ptr){
	pop(reg(RSV_RS2_REG), stack_ptr);
	pop(reg(RSV_RS1_REG), stack_ptr);

	asm_arithmetic(opcode, reg(RSV_RD_REG), reg2space(reg(RSV_RS1_REG)), reg2space(reg(RSV_RS2_REG)));
	push(reg(RSV_RD_REG), stack_ptr);
}

// int is_bottom_pre(AST *root){
// 	AST *temp = root;
// 	while(temp->kind != PREDEC && temp->kind != PREINC){
// 		if(temp->mid == NULL){
// 			temp = NULL;
// 			break;
// 		}else{
// 			temp = temp->mid;
// 		}
// 	}
// 	return temp;
// }

// void arithmetic_sp(OPCODE opcode, int *stack_ptr, AST *root){
// 	Register rs1 = reg(RSV_RS1_REG), rs2 = reg(RSV_RS2_REG);
// 	AST *rhs_bottom = root->rhs, *lhs_bottom = root->lhs;
// 	rhs_bottom = is_bottom_pre(rhs_bottom);
// 	lhs_bottom = is_bottom_pre(lhs_bottom);

// 	if(rhs_bottom != NULL){
// 		rs2 = iden2reg_r(rhs_bottom->mid->val);
// 	}else{
// 		pop(rs2, stack_ptr);
// 	}
// 	if(lhs_bottom != NULL){
// 		rs1 = iden2reg_r(lhs_bottom->mid->val);
// 	}else{
// 		pop(rs1, stack_ptr);
// 	}

// 	asm_arithmetic(opcode, reg(RSV_RD_REG), reg2space(rs1), reg2space(rs2));
// 	push(reg(RSV_RD_REG), stack_ptr);
// }

void inc_dec(AST *root, int *stack_ptr, int is_sub, int is_post){
	AST *tmp = root->mid;
	while (tmp->kind != IDENTIFIER) tmp = tmp->mid;

	Constant c0, c1;
	c0.c = 0;
	c1.c = 1;
	Register target_reg = iden2reg(tmp->val);
	// Register target_reg = iden2reg_w(tmp->val), source_reg = iden2reg_r(tmp->val), temp_reg = reg(RSV_RD_REG);
	OPCODE opcode = OP_ADD; 

	if(is_sub){
		opcode = OP_SUB;
	}
	if(is_post){
		// Postfix Increment/Decrement
		push(target_reg, stack_ptr);
		asm_arithmetic(opcode, target_reg, reg2space(target_reg), const2space(c1));

		// push(source_reg, stack_ptr);
		// asm_arithmetic(opcode, target_reg, reg2space(target_reg), const2space(c1));
		// asm_arithmetic(OP_ADD, source_reg, reg2space(target_reg), const2space(c0));
	}else{
		// Prefix Increment/Decrement
		asm_arithmetic(opcode, target_reg, reg2space(target_reg), const2space(c1));
		push(target_reg, stack_ptr);

		// asm_arithmetic(opcode, target_reg, reg2space(target_reg), const2space(c1));
		// asm_arithmetic(opcode, temp_reg, reg2space(source_reg), const2space(c1));
		// push(temp_reg, stack_ptr);
	}
}

// void inc_dec_sp(AST *root, int *stack_ptr, int is_sub, int is_post){
// 	AST *tmp = root->mid;
// 	while (tmp->kind != IDENTIFIER) tmp = tmp->mid;

// 	Constant c0, c1;
// 	c0.c = 0;
// 	c1.c = 1;
// 	// Register target_reg = iden2reg(tmp->val);
// 	Register target_reg = iden2reg_w(tmp->val), source_reg = iden2reg_r(tmp->val), temp_reg = reg(RSV_RD_REG);
// 	OPCODE opcode = OP_ADD; 

// 	if(is_sub){
// 		opcode = OP_SUB;
// 	}
// 	if(is_post){
// 		// Postfix Increment/Decrement
// 		// push(target_reg, stack_ptr);
// 		// asm_arithmetic(opcode, target_reg, reg2space(target_reg), const2space(c1));

// 		push(source_reg, stack_ptr);
// 		asm_arithmetic(opcode, target_reg, reg2space(target_reg), const2space(c1));
// 		asm_arithmetic(OP_ADD, source_reg, reg2space(target_reg), const2space(c0));
// 	}else{
// 		// Prefix Increment/Decrement
// 		// asm_arithmetic(opcode, target_reg, reg2space(target_reg), const2space(c1));
// 		// push(target_reg, stack_ptr);

// 		asm_arithmetic(opcode, target_reg, reg2space(target_reg), const2space(c1));
// 		asm_arithmetic(opcode, temp_reg, reg2space(source_reg), const2space(c1));
// 		push(temp_reg, stack_ptr);
// 	}
// }

void generate_code(AST *root, int *stack_ptr, CODE_GEN_MODE mode){
	if(root == NULL){
		return;
	}
	AST *next_l = root->lhs, *next_r = root->rhs;
	
	
	CODE_GEN_MODE next_mode = COMPUTE_MODE;
	if(root->kind == ASSIGN){
		// If assining, right to left
		next_mode = ASSIGN_MODE;	
		generate_code(next_r, stack_ptr, COMPUTE_MODE);
		generate_code(next_l, stack_ptr, next_mode);
	}else{
		// If computing, left to right
		generate_code(next_l, stack_ptr, COMPUTE_MODE);
		generate_code(next_r, stack_ptr, COMPUTE_MODE);
	}

	if(root->kind == ASSIGN){
		AST *tmp = root->lhs;
		while (tmp->kind == LPAR) tmp = tmp->mid;

		assign(tmp->val, stack_ptr);
	}else if(root->kind == ADD){
		arithmetic(OP_ADD, stack_ptr);
	}else if(root->kind == SUB){
		arithmetic(OP_SUB, stack_ptr);
	}else if(root->kind == MUL){
		arithmetic(OP_MUL, stack_ptr);
	}else if(root->kind == DIV){
		arithmetic(OP_DIV, stack_ptr);
	}else if(root->kind == REM){
		arithmetic(OP_REM, stack_ptr);
	}else if(root->kind == PREINC){
		inc_dec(root, stack_ptr, 0, 0);
	}else if(root->kind == PREDEC){
		inc_dec(root, stack_ptr, 1, 0);
	}else if(root->kind == POSTINC){
		inc_dec(root, stack_ptr, 0, 1);
	}else if(root->kind == POSTDEC){
		inc_dec(root, stack_ptr, 1, 1);
	}else if(root->kind == IDENTIFIER){
		if(mode == COMPUTE_MODE)
		push(iden2reg(root->val), stack_ptr);
			// push(iden2reg_r(root->val), stack_ptr);
		return;
	}else if(root->kind == CONSTANT){
		set_reg(reg(RSV_RD_REG), root->val);
		push(reg(RSV_RD_REG), stack_ptr);
		return;
	}else if(root->kind == LPAR){
		generate_code(root->mid, stack_ptr, mode);
	}else if(root->kind == RPAR){
		generate_code(root->mid, stack_ptr, mode);
	}else if(root->kind == PLUS){
		generate_code(root->mid, stack_ptr, COMPUTE_MODE);
	}else if(root->kind == MINUS){
		// Traverse down
		generate_code(root->mid, stack_ptr, COMPUTE_MODE);
		
		Constant c1;
		c1.c = 0;
		Register target_reg = reg(RSV_RD_REG), src_reg = reg(RSV_RS2_REG);

		// Pop top to r2
		pop(src_reg, stack_ptr);
		asm_arithmetic(OP_SUB, target_reg, const2space(c1), reg2space(src_reg));

		// Push rd to top
		push(target_reg, stack_ptr);
	}else if(root->kind == END){
		return;	
	}else{
		char buf[CUSTOM_ERR_LEN] = {0};
		sprintf(buf, "Error: No such symbol, root->kind type error: %d\n", root->kind);
		custom_err(buf);
		return;
	}
}

void codegen(AST *root) {
	// TODO: Implement your codegen in your own way.
	// You may modify the function parameter or the return type, even the whole structure as you wish.
	if(root == NULL){
		return;
	}
	
	AST *next_l = root->lhs, *next_r = root->rhs;
	int stack_ptr = mem_addr(3);

	// Load to register
	asm_load(iden2reg(IDEN_X), iden2mem(IDEN_X));
	asm_load(iden2reg(IDEN_Y), iden2mem(IDEN_Y));
	asm_load(iden2reg(IDEN_Z), iden2mem(IDEN_Z));
	
	// Registers for reading
	// asm_load(iden2reg_r(IDEN_X), iden2mem(IDEN_X));
	// asm_load(iden2reg_r(IDEN_Y), iden2mem(IDEN_Y));
	// asm_load(iden2reg_r(IDEN_Z), iden2mem(IDEN_Z));
	// Registers for writing
	// asm_load(iden2reg_w(IDEN_X), iden2mem(IDEN_X));
	// asm_load(iden2reg_w(IDEN_Y), iden2mem(IDEN_Y));
	// asm_load(iden2reg_w(IDEN_Z), iden2mem(IDEN_Z));

	generate_code(root, &stack_ptr, COMPUTE_MODE);

	// Store back to memory
	asm_store(iden2mem(IDEN_X), iden2reg(IDEN_X));
	asm_store(iden2mem(IDEN_Y), iden2reg(IDEN_Y));
	asm_store(iden2mem(IDEN_Z), iden2reg(IDEN_Z));

	// Registers for writing
	// asm_store(iden2mem(IDEN_X), iden2reg_w(IDEN_X));
	// asm_store(iden2mem(IDEN_Y), iden2reg_w(IDEN_Y));
	// asm_store(iden2mem(IDEN_Z), iden2reg_w(IDEN_Z));
}

void freeAST(AST *now) {
	if (now == NULL) return;
	freeAST(now->lhs);
	freeAST(now->mid);
	freeAST(now->rhs);
	free(now);
}

void token_print(Token *in, size_t len) {
	const static char KindName[][20] = {
		"Assign", "Add", "Sub", "Mul", "Div", "Rem", "Inc", "Dec", "Inc", "Dec", "Identifier", "Constant", "LPar", "RPar", "Plus", "Minus", "End"
	};
	const static char KindSymbol[][20] = {
		"'='", "'+'", "'-'", "'*'", "'/'", "'%'", "\"++\"", "\"--\"", "\"++\"", "\"--\"", "", "", "'('", "')'", "'+'", "'-'"
	};
	const static char format_str[] = "<Index = %3d>: %-10s, %-6s = %s\n";
	const static char format_int[] = "<Index = %3d>: %-10s, %-6s = %d\n";
	for(int i = 0; i < len; i++) {
		switch(in[i].kind) {
			case LPAR:
			case RPAR:
			case PREINC:
			case PREDEC:
			case ADD:
			case SUB:
			case MUL:
			case DIV:
			case REM:
			case ASSIGN:
			case PLUS:
			case MINUS:
				printf(format_str, i, KindName[in[i].kind], "symbol", KindSymbol[in[i].kind]);
				break;
			case CONSTANT:
				printf(format_int, i, KindName[in[i].kind], "value", in[i].val);
				break;
			case IDENTIFIER:
				printf(format_str, i, KindName[in[i].kind], "name", (char*)(&(in[i].val)));
				break;
			case END:
				printf("<Index = %3d>: %-10s\n", i, KindName[in[i].kind]);
				break;
			default:
				puts("=== unknown token ===");
		}
	}
}

void AST_print(AST *head) {
	static char indent_str[MAX_LENGTH] = "";
	static int indent = 0;
	char *indent_now = indent_str + indent;
	const static char KindName[][20] = {
		"Assign", "Add", "Sub", "Mul", "Div", "Rem", "PreInc", "PreDec", "PostInc", "PostDec", "Identifier", "Constant", "Parentheses", "Parentheses", "Plus", "Minus"
	};
	const static char format[] = "%s\n";
	const static char format_str[] = "%s, <%s = %s>\n";
	const static char format_val[] = "%s, <%s = %d>\n";
	if (head == NULL) return;
	indent_str[indent - 1] = '-';
	printf("%s", indent_str);
	indent_str[indent - 1] = ' ';
	if (indent_str[indent - 2] == '`')
		indent_str[indent - 2] = ' ';
	switch (head->kind) {
		case ASSIGN:
		case ADD:
		case SUB:
		case MUL:
		case DIV:
		case REM:
		case PREINC:
		case PREDEC:
		case POSTINC:
		case POSTDEC:
		case LPAR:
		case RPAR:
		case PLUS:
		case MINUS:
			printf(format, KindName[head->kind]);
			break;
		case IDENTIFIER:
			printf(format_str, KindName[head->kind], "name", (char*)&(head->val));
			break;
		case CONSTANT:
			printf(format_val, KindName[head->kind], "value", head->val);
			break;
		default:
			puts("=== unknown AST type ===");
	}
	indent += 2;
	strcpy(indent_now, "| ");
	AST_print(head->lhs);
	strcpy(indent_now, "` ");
	AST_print(head->mid);
	AST_print(head->rhs);
	indent -= 2;
	(*indent_now) = '\0';
}
