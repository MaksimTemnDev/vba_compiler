%{

%}

%%


%%
for_stmt : FOR_‘(‘_expr‘;’’expr’;’’expr’)’stmt
	| FOR ‘(’_var_decl ’,’expr’;’expr’)’ stmt
	
	;
stmt : ‘,’
         | ‘,’
         | if_stmt
         | for_stmt
         | …’{’ stmt_list ‘}’				






%right ‘=’
%left ’-’,’+’
%left ‘*’,’/’,’%’
%left ‘.’
%left ‘[’,’]’
%left UMINUS
%nonasoc‘(’,’)’
%%
expr: CONST_INT
        | CONST_STRING
        | CONST_DOUBLE
        | CONST_FLOAT
        | ID
        | expr ‘+’ expr
        | expr ‘-’ expr
        | expr ‘*’ expr
        | expr ‘-’
        | expr ‘/’ expr
        | expr ‘=’ ‘(’ ’[‘ expr‘]’’ )’
        | ‘(‘ expr‘ )’
        | ‘[’expr’]’
        | expr’.’ID
        | ID’(’expr-list’)’
        | expr’:’ID’(’expr-list’)’
        | ‘-’expr ‘%’ prec UMINUS

%%