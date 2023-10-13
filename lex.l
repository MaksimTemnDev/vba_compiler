%option noyywrap
%option never-interactive

DIGIT [0-9]


%x STRING
%x ARRAY
%x NUMBER

%{
	#pragma warning(disable : 4996)
	#include <stdio.h>
    #include <stdlib.h>
    #include <string.h>
    #include <ctype.h>
	

	void removeUnderline();
    void removeType(char *typeName);
	void convertCharacterCodeToString(char characterCode[], int startPosition, char *string);
	char str[1024];
    int lineNumber = 1;
%}

%%

%{
    
%}

\n { lineNumber++; }
" " { }

As { printf("Found KW_AS: %s\n", yytext); }
And { printf("Found KW_AND: %s\n", yytext); }
Binary { printf("Found KW_BINARY: %s\n", yytext); }
Continue { printf("Found KW_CONTINUE: %s\n", yytext); }
ByRef { printf("Found KW_BYREF: %s\n", yytext); }
ByVal { printf("Found KW_BYVAL: %s\n", yytext); }
Do { printf("Found KW_DO: %s\n", yytext); }
Else { printf("Found KW_ELSE: %s\n", yytext); }
ElseIf { printf("Found KW_ELSEIF: %s\n", yytext); }
End { printf("Found KW_END: %s\n", yytext); }
Enum { printf("Found KW_ENUM: %s\n", yytext); }
Exit { printf("Found KW_EXIT: %s\n", yytext); }
Empty { printf("Found KW_EMPTY: %s\n", yytext); }
False { printf("Found KW_FALSE: %s\n", yytext); }
Error { printf("Found KW_ERROR: %s\n", yytext); }
For { printf("Found KW_FOR: %s\n", yytext); }
If { printf("Found KW_IF: %s\n", yytext); }
Friend { printf("Found KW_FRIEND: %s\n", yytext); }
Get { printf("Found KW_GET: %s\n", yytext); }
Let { printf("Found KW_LET: %s\n", yytext); }
Is { printf("Found KW_IS: %s\n", yytext); }
Len { printf("Found KW_LEN: %s\n", yytext); }
Lock { printf("Found KW_Lock: %s\n", yytext); }
Me { printf("Found KW_ME: %s\n", yytext); }
Mid { printf("Found KW_MID: %s\n", yytext); }
Mod { printf("Found KW_MOD: %s\n", yytext); }
New { printf("Found KW_NEW: %s\n", yytext); }
Next { printf("Found KW_NEXT: %s\n", yytext); }
Null { printf("Found KW_NULL: %s\n", yytext); }
Return { printf("Found KW_RETURN: %s\n", yytext); }
On { printf("Found KW_ON: %s\n", yytext); }
Option { printf("Found KW_OPTION: %s\n", yytext); }
Optional { printf("Found KW_OPTIONAL: %s\n", yytext); }
Or { printf("Found KW_OR: %s\n", yytext); }
ParamArray { printf("Found KW_PARAMARRAY: %s\n", yytext); }
Print { printf("Found KW_PRINT: %s\n", yytext); }
Private { printf("Found KW_PRIVATE: %s\n", yytext); }
Protected { printf("Found KW_PROTECTED: %s\n", yytext); }
Property { printf("Found KW_PROPERTY: %s\n", yytext); }
PtrSafe { printf("Found KW_PTRSAFE: %s\n", yytext); }
Public { printf("Found KW_PUBLIC: %s\n", yytext); }
Resume { printf("Found KW_RESUME: %s\n", yytext); }
Seek { printf("Found KW_SEEK: %s\n", yytext); }
Set { printf("Found KW_SET: %s\n", yytext); }
Static { printf("Found KW_STATIC: %s\n", yytext); }
True  { printf("Found KW_TRUE: %s\n", yytext); }
Step { printf("Found KW_STEP: %s\n", yytext); }
Then { printf("Found KW_THEN: %s\n", yytext); }
Time { printf("Found KW_TIME: %s\n", yytext); }
To { printf("Found KW_TO: %s\n", yytext); }
While { printf("Found KW_WHILE: %s\n", yytext); }
Where { printf("Found KW_WHERE: %s\n", yytext); }
Loop { printf("Found KW_LOOP: %s\n", yytext); }
Xor { printf("Found KW_Xor: %s\n", yytext); }

Boolean { printf("Found TYPE_BOOLEAN: %s\n", yytext); }
Byte { printf("Found TYPE_BYTE: %s\n", yytext); }
Integer { printf("Found TYPE_INTEGER: %s\n", yytext); }
Single { printf("Found TYPE_SINGLE: %s\n", yytext); }
Short { printf("Found TYPE_SHORT: %s\n", yytext); }
Double { printf("Found TYPE_DOUBLE: %s\n", yytext); }
Decimal { printf("Found TYPE_DECIMAL: %s\n", yytext); }
Date { printf("Found TYPE_DATE: %s\n", yytext); }
Char { printf("Found TYPE_CHAR: %s\n", yytext); }
String { printf("Found TYPE_STRING: %s\n", yytext); }
Object { printf("Found TYPE_OBJECT: %s\n", yytext); }

vbCrLf|(Chr\(13\)\s\+\sChr\(10\)) { printf("Found TYPE_CR_LF: %s\n", yytext); }
vbCr|(Chr\(13\)) { printf("Found TYPE_CR: %s\n", yytext); }
vbLf|(Chr\(10\)) { printf("Found TYPE_LF: %s\n", yytext); }
vbNewLine|(Chr\(13\)\s\+\sChr\(10\)) { printf("Found TYPE_NEW_LINE: %s\n", yytext); }
vbNullChar|(Chr\(0\)) { printf("Found TYPE_NULL_CHAR: %s\n", yytext); }
vbNullString { printf("Found TYPE_NULL_STRING: %s\n", yytext); }
vbObjectError|(-2147221504) { printf("Found TYPE_OBJECT_ERROR: %s\n", yytext); }
vbTab|(Chr\(9\)) { printf("Found TYPE_TAB: %s\n", yytext); }
vbBack|(Chr\(8\)) { printf("Found TYPE_BACK: %s\n", yytext); }

"+" { printf("Found OPERATOR_PLUS: %s\n", yytext); }
"-" { printf("Found OPERATOR_MINUS: %s\n", yytext); }
"*" { printf("Found OPERATOR_MUL: %s\n", yytext); }
"/" { printf("Found OPERATOR_DIV: %s\n", yytext); }
"^" { printf("Found OPERATOR_EXPONENT: %s\n", yytext); }
"\\" { printf("Found OPERATOR_INTEGER_DIVISION: %s\n", yytext); }

"=" { printf("Found OPERATOR_ASSIGNMENT: %s\n", yytext); }
"==" { printf("Found OPERATOR_EQUAL: %s\n", yytext); }
"<>" { printf("Found OPERATOR_NOT_EQUAL: %s\n", yytext); }
"<" { printf("Found OPERATOR_LESS: %s\n", yytext); }
">" { printf("Found OPERATOR_MORE: %s\n", yytext); }
"<=" { printf("Found OPERATOR_LESS_OR_EQUAL: %s\n", yytext); }
">=" { printf("Found OPERATOR_MORE_OR_EQUAL: %s\n", yytext); }
"<<" { printf("Found OPERATOR_BIT_LEFT_SHIFT: %s\n", yytext); }
">>" { printf("Found OPERATOR_BIT_RIGHT_SHIFT: %s\n", yytext); }

"+=" { printf("Found OPERATOR_PLUS_ASSIGNMENT: %s\n", yytext); }
"-=" { printf("Found OPERATOR_MINUS_ASSIGNMENT: %s\n", yytext); }
"*=" { printf("Found OPERATOR_MUL_ASSIGNMENT: %s\n", yytext); }
"/=" { printf("Found OPERATOR_DIV_ASSIGNMENT: %s\n", yytext); }
"^=" { printf("Found OPERATOR_EXP_ASSIGNMENT: %s\n", yytext); }
"&=" { printf("Found OPERATOR_BIT_AND_ASSIGNMENT: %s\n", yytext); }
"\\=" { printf("Found OPERATOR_DIV_NUM_ASSIGNMENT: %s\n", yytext); }
"<<=" { printf("Found OPERATOR_BIT_LEFT_SHIFT_ASSIGNMENT: %s\n", yytext); }
">>=" { printf("Found OPERATOR_BIT_RIGHT_SHIFT_ASSIGNMENT: %s\n", yytext); }

"." { printf("Found OPERATOR_POINT: %s\n", yytext); }

"(" { printf("Found LEFT_BRACKET: %s\n", yytext); }
")" { printf("Found RIGHT_BRACKET: %s\n", yytext); }
"[" { printf("Found LEFT_SQUARE_BRACKET: %s\n", yytext); }
"]" { printf("Found RIGHT_SQUARE_BRACKET: %s\n", yytext); }

"," { printf("Found COMMA: %s\n", yytext); }


'[^\n]* { printf("Found COMMENT: %s\n", yytext); }
 
 \" { strcpy(str,""); BEGIN(STRING); }
<STRING>[^\\\"\n]+ { strcat(str,yytext); }
<STRING>\n  { lineNumber++; }
<STRING>\\n  { strcat(str,"\n"); }
<STRING>\\r  { strcat(str,"\r"); }
<STRING>\\t  { strcat(str,"\t"); }
<STRING>\\0  { strcat(str,"\0"); }
<STRING>\\\\ { strcat(str,"\\"); }
<STRING>\\\" { strcat(str,"\""); }
<STRING>\\x[0-7][a-zA-Z0-9] {
    char string[2];
    convertCharacterCodeToString(yytext, 2, string);
    strcat(str, string);
}
<STRING>\" { printf("Found STRING: %s\n", str); BEGIN(INITIAL);}
<STRING><<EOF>> { printf("STRING Error in line %d: there is no closing quotation mark\n", lineNumber); BEGIN(INITIAL);}
<STRING>\\ { printf("STRING Error in line %d: slash", lineNumber); }

\{ {strcpy(str,"["); BEGIN(ARRAY); }
<ARRAY>[^}]+ { strcat(str,yytext); }
<ARRAY>\} { printf("Found ARRAY: %s]\n", str); BEGIN(INITIAL);}
<ARRAY><<EOF>> { printf("ARRAY Error in line %d: there is no closing brace\n", lineNumber); BEGIN(INITIAL);}



[0-9][_0-9]*(Decimal)? {

    removeType("Decimal");
    removeUnderline();
    printf("Found DECIMAL_NUMBER: %d\n", atoi(str));
}

0b([_0-1])+(Byte)? {

    removeType("Byte");
    removeUnderline();
    if(strlen(str) > 2)
    {
         printf("Found BYTE_NUMBER: %d\n", strtol(str + 2, NULL, 2));
    }
    else
    {
        printf("NUMBER Error in line %d: incorrect format\n", lineNumber);
    }
}

[0-9][_0-9]*\.([0-9][_0-9]*([eE][+-]?[0-9][_0-9]*)?)?(Double)? {
    removeType("Double");
    removeUnderline();
    printf("Found DOUBLE: %lf\n", atof(str), str);
}

[0-9][_0-9]*([eE][+-]?[0-9][_0-9]*)?(Double)? {
    removeType("Double");
    removeUnderline();
    printf("Found DOUBLE: %lf\n", atof(str));
}

[a-zA-Z_][a-zA-Z0-9_]* { printf("Found IDENTIFIER: %s\n", yytext);}
[a-zA-Z0-9_]+\(\d\) { printf("Found ARRAY_ELEMENT_ACCESS_OPERATOR: %s\n", yytext); }

%%

void convertCharacterCodeToString(char characterCode[], int startPosition, char *string)
{
    string[0] = strtol(characterCode + startPosition,NULL, 16);
    string[1] = '\0';
}

void removeUnderline()
{
  int pos = 0;
  for(int i = 0; i < strlen(yytext); i++)
  {
        if(yytext[i] != '_')
        {
            str[pos++] = yytext[i];
        }
  }

  str[pos] = '\0';
  strcpy(yytext, str);
}

void removeType(char *typeName)
{
    strcpy(str, yytext);
    char* p = strstr(str, typeName);
    if(p != NULL)
    {
        int pos = strlen(str)  - strlen(typeName);
        str[pos] = 0;
    }
}

int main(int argc, char** argv) {

    if (argc != 2) {
        printf("Filename is not found");
        return 1;
    }

    const char *filename= argv[1];
    
    FILE *file = fopen(filename, "r");
    if (file == NULL) {
        printf("File opening error");
        return 1;
    }

    yyin = file;
    yylex();
    fclose(file);

    return 0;
}