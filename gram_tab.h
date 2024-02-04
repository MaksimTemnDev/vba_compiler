#pragma once
#include "classesnodes.h"
typedef union {
	string* string_literal;
	string* identifier;
	bool bool_literal;
	double double_literal;
	char char_literal;
	int decimal_number;

	CodeNode* code;
	ExprNode* expr;
	ExprListNode* expr_list;
	StmtNode* stmt;
	StmtListNode* stmt_list;
	FuncDecl* func_decl;
	FuncParamNode* function_param;
	FuncParamListNode* function_params;
	TypeNode* type;
	GlobalCodeList* globalCodeList;
	GlobalCode* globalCode;
	StaticDim* static_;
	DimStmt* dimStmt;
	Value* value;
	ForNode* forNode;
	ArrayIdList* arrayIdList;
	ArrayIdDeclare* arrayIdDeclare;
	Identificator* identificator;
	IdList* idList;
	While* whileSt;
	Ternar* ternar;
	IfNode* ifNode;
	OptionalStep* optStep;
	BodyStmt* bodyStmt;
} YYSTYPE;
#define	STRING	258
#define	IDENTIFIER	259
#define	DOUBLE	260
#define	CHAR	261
#define	DECIMAL_NUMBER	262
#define	END	263
#define	WHILE	264
#define	DO	265
#define	LOOP	266
#define	UNTIL	267
#define	FOR	268
#define	TO	269
#define	STEP	270
#define	CONTINUE	271
#define	EXIT	272
#define	IF	273
#define	THEN	274
#define	ELSE	275
#define	ELSEIF	276
#define	DIM	277
#define	ByRef	278
#define	ByVal	279
#define	NEW	280
#define	RETURN	281
#define	NEXT	282
#define	As	283
#define	TYPE_BOOLEAN	284
#define	TYPE_BYTE	285
#define	TYPE_INTEGER	286
#define	TYPE_SINGLE	287
#define	TYPE_SHORT	288
#define	TYPE_DOUBLE	289
#define	TYPE_DECIMAL	290
#define	TYPE_DATE	291
#define	TYPE_CHAR	292
#define	TYPE_STRING	293
#define	TYPE_OBJECT	294
#define	TOKEN_LINE	295
#define	Function	296
#define	Sub	297
#define	KW_STATIC	298
#define	KW_FALSE	299
#define	KW_TRUE	300
#define	PLUS_ASSIGNMENT	301
#define	MINUS_ASSIGNMENT	302
#define	MUL_ASSIGNMENT	303
#define	DIV_ASSIGNMENT	304
#define	EXP_ASSIGNMENT	305
#define	BIT_AND_ASSIGNMENT	306
#define	DIV_NUM_ASSIGNMENT	307
#define	BIT_LEFT_SHIFT_ASSIGNMENT	308
#define	BIT_RIGHT_SHIFT_ASSIGNMENT	309
#define	UnarPlus	310
#define	UnarMinus	311
#define	MOD	312
#define	BIT_LEFT_SHIFT	313
#define	BIT_RIGHT_SHIFT	314
#define	NOT_EQUAL	315
#define	LESS_OR_SAME	316
#define	Is	317
#define	IsNot	318
#define	Like	319
#define	TypeOf	320
#define	Not	321
#define	MORE_OR_SAME	322
#define	OR	323
#define	ORELSE	324
#define	AND	325
#define	ANDALSO	326


extern YYSTYPE yylval;
