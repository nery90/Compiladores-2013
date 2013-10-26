/*
 *	Analizador Léxico	
 *	Curso: Compiladores y Lenguajes de Bajo de Nivel
 *	Práctica de Programación Nro. 1
 *	
 *	Descripcion:
 *	Implementa un analizador léxico que reconoce números, identificadores, 
 * 	palabras reservadas, operadores y signos de puntuación para un lenguaje
 * 	con sintaxis tipo C.
 *	
 * 	Despliega cada linea con los lexemas -> token
 */

/*********** Librerias utilizadas **************/

#include<stdio.h>
#include<string.h>
#include<stdlib.h>
#include<ctype.h>

/***************** MACROS **********************/

//INPUTS
#define SB_SEP		271
#define SB_SPLN		272
#define SB_PRAB		273
#define SB_PRCR		274
#define SB_CHAB		275
#define SB_CHCR		276
#define SB_VAR          278
#define SB_IF           279
#define SB_THEN         280
#define SB_FOR          281
#define SB_ELSE         282
#define SB_END          283

#define SB_TO           298
#define SB_STEP         299
#define SB_DO           300
#define SB_CONST        301
#define SB_INT          302
#define SB_VOID         303
#define SB_FLOAT        304
#define SB_DOUBLE       305
#define SB_CHAR         306
#define SB_TRUE         307
#define SB_FALSE        308


#define OP_AND		284
#define OP_OR		285
#define OP_NOT		286
#define OP_REL		287
#define OP_ADD      289
#define OP_MULT		290
#define OP_DIV		290
#define OP_ASIG		292

#define OP_EQ       309

#define LIT_USER	293 //cadena de caracteres
#define LITV_USER	294 //cadena vacía
#define CAR_USER	295 //caracter 
#define CRV_USER 	296 //caracter vacío
#define NUM_USER	297
#define ID_USER		400

// Fin INPUTS
#define TAMBUFF 	5
#define TAMLEX 		50
#define TAMHASH 	101

/************* Definiciones ********************/

typedef struct entrada{
	//definir los campos de 1 entrada de la tabla de simbolos
	int compLex;
	char lexema[TAMLEX];	
	struct entrada *tipoDato; // null puede representar variable no declarada	
	// aqui irian mas atributos para funciones y procedimientos...
	
} entrada;

typedef struct {
	int compLex;
	entrada *pe;
} token;

/************* Variables globales **************/

int consumir;			/* 1 indica al analizador lexico que debe devolver
						el sgte componente lexico, 0 debe devolver el actual */

char cad[5*TAMLEX];		// string utilizado para cargar mensajes de error
token t;				// token global para recibir componentes del Analizador Lexico

// variables para el analizador lexico

FILE *archivo;			// Fuente con Expresiones aritméticas
char buff[2*TAMBUFF];	// Buffer para lectura de archivo fuente
char id[TAMLEX];		// Utilizado por el analizador lexico
int delantero=-1;		// Utilizado por el analizador lexico
int fin=0;				// Utilizado por el analizador lexico
int numLinea=1;			// Numero de Linea
/************** Prototipos *********************/

void sigLex();		// Del analizador Lexico

/**************** Funciones **********************/
int stricmp(const char* cad1,const char* cad2) 
{
    int cmp;
    size_t i;

    char* c1 = strdup(cad1);
    char* c2 = strdup(cad2);

    const size_t s1 = strlen(c1);
    const size_t s2 = strlen(c2);

    for(i = 0; i < s1; ++i)
        c1[i] = toupper(c1[i]);

    for(i = 0; i < s2; ++i)
        c2[i] = toupper(c2[i]);

    cmp = strcmp(c1, c2);

    free(c2);
    free(c1);

    return cmp;
}

/*********************HASH************************/
entrada *tabla;				//declarar la tabla de simbolos
int tamTabla=TAMHASH;		//utilizado para cuando se debe hacer rehash
int elems=0;				//utilizado para cuando se debe hacer rehash

int h(const char* rjk, int m)
{
	unsigned h=0,g;
	int i;
    char* k = strdup(rjk);
    const size_t sz = strlen(k);
    for(i = 0; i < sz; ++i)
        k[i] = toupper(k[i]);
	for (i=0;i<strlen(k);i++)
	{
		h=(h << 4) + k[i];
		if (g=h&0xf0000000)
		{
			h=h^(g>>24);
			h=h^g;
		}
	}
    free(k);
	return h%m;
}
void insertar(entrada e);

void initTabla()
{	
	int i=0;
	
	tabla=(entrada*)malloc(tamTabla*sizeof(entrada));
	for(i=0;i<tamTabla;i++)
	{
		tabla[i].compLex=-1;
	}
}

int esprimo(int n)
{
	int i;
	for(i=3;i*i<=n;i+=2)
		if (n%i==0)
			return 0;
	return 1;
}

int siguiente_primo(int n)
{
	if (n%2==0)
		n++;
	for (;!esprimo(n);n+=2);

	return n;
}

//en caso de que la tabla llegue al limite, duplicar el tamaño
void rehash()
{
	entrada *vieja;
	int i;
	vieja=tabla;
	tamTabla=siguiente_primo(2*tamTabla);
	initTabla();
	for (i=0;i<tamTabla/2;i++)
	{
		if(vieja[i].compLex!=-1)
			insertar(vieja[i]);
	}		
	free(vieja);
}

//insertar una entrada en la tabla
void insertar(entrada e)
{
	int pos;
	if (++elems>=tamTabla/2)
		rehash();
	pos=h(e.lexema,tamTabla);
	while (tabla[pos].compLex!=-1)
	{
		pos++;
		if (pos==tamTabla)
			pos=0;
	}
	tabla[pos]=e;

}
//busca una clave en la tabla, si no existe devuelve NULL, posicion en caso contrario
entrada* buscar(const char *clave)
{
	int pos;
	entrada *e;
	pos=h(clave,tamTabla);
	while(tabla[pos].compLex!=-1 && stricmp(tabla[pos].lexema,clave)!=0 )
	{
		pos++;
		if (pos==tamTabla)
			pos=0;
	}
	return &tabla[pos];
}

void insertTablaSimbolos(const char *s, int n)
{
	entrada e;
	sprintf(e.lexema,s);
	e.compLex=n;
	insertar(e);
}

void initTablaSimbolos()
{
	int i;
	entrada pr,*e;
	
	 insertTablaSimbolos("true", SB_IF);
         insertTablaSimbolos("false", SB_IF);
         insertTablaSimbolos("if", SB_IF);
         insertTablaSimbolos("then",SB_THEN);
         insertTablaSimbolos("for",SB_FOR);
         insertTablaSimbolos("else",SB_ELSE);
         insertTablaSimbolos("end",SB_END);
         insertTablaSimbolos("to",SB_TO);
         insertTablaSimbolos("step",SB_STEP);
         insertTablaSimbolos("do",SB_DO);
        insertTablaSimbolos("var",SB_VAR);
 	insertTablaSimbolos(",",SB_SEP);
	insertTablaSimbolos(";",SB_SPLN);
	insertTablaSimbolos("(",SB_PRAB);
	insertTablaSimbolos(")",SB_PRCR);
	insertTablaSimbolos("[",SB_CHAB);
	insertTablaSimbolos("]",SB_CHCR);
	insertTablaSimbolos("!",OP_NOT);
	insertTablaSimbolos("||",OP_OR);	
	insertTablaSimbolos("&&",OP_AND);
	insertTablaSimbolos("<",OP_REL);
	insertTablaSimbolos("<=",OP_REL);
	insertTablaSimbolos("<>",OP_EQ);
	insertTablaSimbolos(">",OP_REL);
	insertTablaSimbolos(">=",OP_REL);
	insertTablaSimbolos("!=",OP_REL);
	insertTablaSimbolos("=",OP_ASIG);
	insertTablaSimbolos("==",OP_EQ);
	insertTablaSimbolos("+",OP_ADD);
	insertTablaSimbolos("-",OP_ADD);
	insertTablaSimbolos("*",OP_MULT);
	insertTablaSimbolos("/",OP_DIV);
}

// Rutinas del analizador lexico

void error(const char* mensaje)
{
	printf(" //Error Lexico. %s.\n",mensaje);	
}

void sigLex()
{
	int i=0;
	char c=0;
	int acepto=0;
	int estado=0;
	char msg[41];
	entrada e;

	while((c=fgetc(archivo))!=EOF)
	{
		if (c==' ' || c=='\t')
			continue;	//eliminar espacios en blanco
		else if(c=='\n')
		{
			numLinea++;//incrementar el numero de linea
			c=fgetc(archivo);
			ungetc(c,archivo);
			continue;
		}
		else if (isalpha(c))
		{
			//es un identificador (o palabra reservada)
			i=0;
			do{
				id[i]=c;
				i++;
				c=fgetc(archivo);
				if (i>=TAMLEX)
					error("Longitud de Identificador excede tamaño de buffer");
			}while(isalpha(c) || isdigit(c));
			id[i]='\0';
			if (c!=EOF)
				ungetc(c,archivo);
			else
				c=0;
			t.pe=buscar(id);
			t.compLex=t.pe->compLex;
			if (t.pe->compLex==-1)
			{
				sprintf(e.lexema,id);
				e.compLex=ID_USER;
				insertar(e);
				t.pe=buscar(id);
				t.compLex=ID_USER;
			}
			break;
		}
		else if (isdigit(c))
		{
				//es un numero
				i=0;
				estado=0;
				acepto=0;
				id[i]=c;
				
				while(!acepto)
				{
					switch(estado){
					case 0: //una secuencia netamente de digitos, puede ocurrir . o e
						c=fgetc(archivo);
						if (isdigit(c))
						{
							id[++i]=c;
							estado=0;
						}
						else if(c=='.'){
							id[++i]=c;
							estado=1;
						}
						else if(tolower(c)=='e'){
							id[++i]=c;
							estado=3;
						}
						else{
							estado=6;
						}
						break;
					
					case 1://un punto, debe seguir un digito
						c=fgetc(archivo);						
						if (isdigit(c))
						{
							id[++i]=c;
							estado=2;
						}
						else{
							sprintf(msg,"No se esperaba '%c'",c);
							estado=-1;
						}
						break;
					case 2://la fraccion decimal, pueden seguir los digitos o e
						c=fgetc(archivo);
						if (isdigit(c))
						{
							id[++i]=c;
							estado=2;
						}
						else if(tolower(c)=='e')
						{
							id[++i]=c;
							estado=3;
						}
						else
							estado=6;
						break;
					case 3://una e, debe proseguir un dígito o una secuencia de digitos
						c=fgetc(archivo);
						if (c=='+' || c=='-')
						{
							id[++i]=c;
							estado=4;
						}
						else if(isdigit(c))
						{
							id[++i]=c;
							estado=5;
						}
						else{
							sprintf(msg,"No se esperaba '%c'",c);
							estado=-1;
						}
						break;
					case 4://necesariamente debe venir por lo menos un digito
						c=fgetc(archivo);
						if (isdigit(c))
						{
							id[++i]=c;
							estado=5;
						}
						else{
							sprintf(msg,"No se esperaba '%c'",c);
							estado=-1;
						}
						break;
					case 5://una secuencia de digitos
						c=fgetc(archivo);
						if (isdigit(c))
						{
							id[++i]=c;
							estado=5;
						}
						else{
							estado=6;
						}break;
					case 6:	//estado de aceptacion, 
							//devolver el caracter correspondiente a otro componente lexico
						if (c!=EOF)
							ungetc(c,archivo);
						else
							c=0;
						id[++i]='\0';
						acepto=1;
						t.pe=buscar(id);
						if (t.pe->compLex==-1)
						{
							sprintf(e.lexema,id);
							e.compLex=NUM_USER;
							insertar(e);
							t.pe=buscar(id);
						}
						t.compLex=NUM_USER;
						break;
					case -1:
						if (c==EOF)
							error("No se esperaba el fin de archivo");
						else
							error(msg);
						exit(1);
					}
				}
			break;
		}
		else if (c=='<') 
		{
			//es un operador relacional, averiguar cual
			c=fgetc(archivo);
			if (c=='>'){
				t.compLex=OP_REL;
				t.pe=buscar("<>");
			}
			else if (c=='='){
				t.compLex=OP_REL;
				t.pe=buscar("<=");
			}
			else{
				ungetc(c,archivo);
				t.compLex=OP_REL;
				t.pe=buscar("<");
			}
			break;
		}
		else if (c=='>')
		{
			//es un operador relacional, averiguar cual
			c=fgetc(archivo);
			if (c=='='){
				t.compLex=OP_REL;
				t.pe=buscar(">=");
			}
			else{
				ungetc(c,archivo);
				t.compLex=OP_REL;
				t.pe=buscar(">");
			}
			break;
		}
		else if (c=='=')
		{
			//es un operador relacional o de asignación
			c=fgetc(archivo);
			if (c=='='){
				t.compLex=OP_REL;
				t.pe=buscar("==");
			}
			else{
				ungetc(c,archivo);
				t.compLex=OP_ASIG;
				t.pe=buscar("=");
			}
			break;
		}
		else if (c=='&')
		{
			//es un operador relacional
			c=fgetc(archivo);
			if (c=='&')
			{
				t.compLex=OP_AND;
				t.pe=buscar("&&");
			}
			else{
				ungetc(c,archivo);
				error("Falta el 2ndo \'&\'");
			}
			break;
		}
		else if (c=='|')
		{
			//es un operador relacional
			c=fgetc(archivo);
			if (c=='|')
			{
				t.compLex=OP_OR;
				t.pe=buscar("||");
			}
			else{
				ungetc(c,archivo);
				error("Falta el 2ndo \'|\'");
			}
			break;
		}
		else if (c=='!')
		{
			//es un operador lógico
			c=fgetc(archivo);
			if (c=='=')
			{
				t.compLex=OP_REL;
				t.pe=buscar("!=");
			}
			else{
				ungetc(c,archivo);
				t.compLex=OP_NOT;
				t.pe=buscar("!");
			}
			break;
		}
		else if (c==','){
			t.compLex=SB_SEP;
			t.pe=buscar(",");
			break;
		}
		else if (c==';'){
			t.compLex=SB_SPLN;
			t.pe=buscar(";");
			break;
		}
		else if (c=='('){
			t.compLex=SB_PRAB;
			t.pe=buscar("(");
			break;
		}
		else if (c==')'){
			t.compLex=SB_PRCR;
			t.pe=buscar(")");
			break;
		}
		else if (c=='['){
			t.compLex=SB_CHAB;
			t.pe=buscar("[");
			break;
		}
		else if (c==']'){
			t.compLex=SB_CHCR;
			t.pe=buscar("]");
			break;
		}
		else if (c=='+'){
			t.compLex=OP_ADD;
			t.pe=buscar("+");
			break;
		}
		else if (c=='-'){
			t.compLex=OP_ADD;
			t.pe=buscar("-");
			break;
		}
		else if (c=='*'){
			t.compLex=OP_MULT;
			t.pe=buscar("*");
			break;
		}
		else if (c=='/'){
			if ((c=fgetc(archivo))=='/')
            {//la secuencia es un comentario
                while(c!=EOF && c!='\n')
                {
                    c=fgetc(archivo);					
                }
                ungetc(c,archivo);
            }
            else {
				ungetc(c,archivo);
                t.compLex=OP_DIV;
                t.pe=buscar("/");
                break;
            }
		}
		else if (c=='\'')
		{//un caracter
			i=0;
			id[i]=c;
			i++;
			do{
				c=fgetc(archivo);
				if (c=='\'')
				{
					id[i]=c;
					i++;
				}
				else if (c!='\'' && c==';'){
					break;
				}
				else if(c==EOF){
					error("Se llego al fin de archivo sin finalizar un literal");
					break;
				}
				else{
					id[i]=c;
					i++;
				}
			}while(isascii(c));
			id[i]='\0';
			if (c!=EOF)
				ungetc(c,archivo);
			else
				c=0;
			t.pe=buscar(id);
			t.compLex=t.pe->compLex;
			if (t.pe->compLex==-1)
			{
				sprintf(e.lexema,id);
				if (strlen(id)==3 || strcmp(id,"''")==0)
					e.compLex=CRV_USER;
				else
					e.compLex=CAR_USER;
				insertar(e);
				t.pe=buscar(id);
				t.compLex=e.compLex;
			}
			break;
		}
		else if (c=='\"')
		{//un caracter o una cadena de caracteres
			i=0;
			id[i]=c;
			i++;
			do{
				c=fgetc(archivo);
				if (c=='\"')
				{
					id[i]=c;
					i++;
                    break;
				}
				else if(c==EOF){
					error("Se llego al fin de archivo sin finalizar un literal");
					break;
				}
				else{
					id[i]=c;
					i++;
				}
			}while(isascii(c));
			id[i]='\0';
			t.pe=buscar(id);
			t.compLex=t.pe->compLex;
			if (t.pe->compLex==-1)
			{
				sprintf(e.lexema,id);
				if (strlen(id)==2 || strcmp(id,"""")==0)
					e.compLex=LITV_USER;
				else
					e.compLex=LIT_USER;
				insertar(e);
				t.pe=buscar(id);
				t.compLex=e.compLex;
			}
			break;
		}
		else if (c!=EOF)
		{
			sprintf(msg,"%c no esperado",c);
			error(msg);
			break;
		}
	}
	if (c==EOF)
	{
		t.compLex=EOF;
		sprintf(e.lexema,"EOF");
		t.pe=&e;
	}
	
}

/////////////////////////////////////////////Syntaxer///////////////////////
void ParsearFuente();
void ParsearPrograma();
void ParsearInstruccion();
void ParsearDeclaracion();
void ParsearListaDeclaracion();
void ParsearVariablesDeclaracion();
void ParsearTamArreglo();
void ParsearBloqueSenctencia();
void ParsearSentencia();
void ParsearSentenciaUser();
void ParsearListaArgumentos();
void ParsearSentenciaIf();
void ParsearSentenciaFor();
void ParsearInicControl();
void ParseareExprMultiplicativa();
void ParsearExprAd();
void ParsearExprRel();
void ParsearExprIgual();
void ParsearExprAnd();
void ParsearExprOr();
void ParsearExprUnaria();
void ParsearExprPrim();
void ParsearExprUsr();
void ParsearExpr();
void ParsearLit();

void Match(int act, int exp)
{
    if (exp == ID_USER)
    {
        if(act>=ID_USER){
            sigLex();
        }
    }
    else if(act==exp){
        sigLex();
    }
    else
    {
      printf("Error inesperado....");
      printf("Se encontro %d, se esperaba %d.\n", act, exp);
      printf("Linea %d.\n", numLinea);
    }
    
}

void ParsearLit()
{
    if (t.compLex==LIT_USER)
        Match(t.compLex,LIT_USER);
    else if (t.compLex==NUM_USER)
        Match(t.compLex,NUM_USER);
    else if (t.compLex==SB_TRUE)
        Match(t.compLex,SB_FALSE);
}

void ParsearExpr()
{
    ParsearExprOr();
}

void ParsearExprUsr()
{
    Match(t.compLex,ID_USER);
  
    if (t.compLex==SB_PRAB)
    {
        Match(t.compLex,SB_PRAB);
        ParsearListaArgumentos();
        Match(t.compLex,SB_PRCR);
    }
    
    else if (t.compLex==SB_CHAB)
    {
        Match(t.compLex,SB_CHAB);
        ParsearExpr();
        Match(t.compLex,SB_CHCR);
    }
}

void ParsearExprPrim()
{
    if (t.compLex==SB_PRAB)
    {
        Match(t.compLex,SB_PRAB);
        ParsearExpr();
        Match(t.compLex,SB_PRCR);
    }
    else if (t.compLex==ID_USER)
    {
        ParsearExprUsr();
    }
    else
    {
        ParsearLit();
    }
}

void ParsearExprUnaria()
{
    while (t.compLex==OP_ADD || t.compLex==OP_NOT )
    {
        if(t.compLex==OP_ADD)
        Match(t.compLex,OP_ADD);
        else
            Match(t.compLex,OP_NOT);
    }
    ParsearExprPrim();
}

void ParsearExprOr()
{
    ParsearExprAnd();
    while (t.compLex==OP_OR)
    {
        Match(t.compLex,OP_OR);
        ParsearExprAnd();
    }
}

void ParsearExprAnd()
{
    ParsearExprIgual();
    while (t.compLex==OP_AND)
    {
        Match(t.compLex,OP_AND);
        ParsearExprIgual();
    }
}

void ParsearExprIgual()
{
    ParsearExprRel();
    while (t.compLex==OP_EQ)
    {
        Match(t.compLex,OP_EQ);
        ParsearExprRel();
    }
}

void ParsearExprRel()
{
    ParsearExprAd();
    while (t.compLex==OP_REL)
    {
        Match(t.compLex,OP_REL);
        ParsearExprAd();
    }
}
 
void ParsearExprAd()
{
    ParseareExprMultiplicativa();
    while (t.compLex==OP_ADD)
    {
        Match(t.compLex,OP_ADD);
        ParseareExprMultiplicativa();
    }
}

void ParseareExprMultiplicativa()
{
    ParsearExprUnaria();
    while (t.compLex==OP_MULT)
    {
        Match(t.compLex,OP_MULT);
        ParsearExprUnaria();
    }
}

void ParsearInicControl()
{
    Match(t.compLex,ID_USER);
    Match(t.compLex,OP_ASIG);
    ParsearExpr();
    Match(t.compLex,SB_TO);
    ParsearExpr();
    Match(t.compLex,SB_STEP);
    ParsearExpr();
}

void ParsearSentenciaFor()
{
    Match(t.compLex,SB_FOR);
    ParsearInicControl();
    Match(t.compLex,SB_DO);
    ParsearBloqueSenctencia();
    Match(t.compLex,SB_END);
    Match(t.compLex,SB_FOR);
}

void ParsearSentenciaIf()
{
    Match(t.compLex,SB_IF);
    ParsearExpr();
    Match(t.compLex,SB_THEN);
    ParsearBloqueSenctencia();
    if (t.compLex==SB_ELSE)
    {
        Match(t.compLex,SB_ELSE);
        ParsearBloqueSenctencia();
    }
    Match(t.compLex,SB_END);
    Match(t.compLex,SB_IF);
}

void ParsearListaArgumentos()
{
    if (t.compLex!=SB_PRCR)
    {
        ParsearExpr();
        if (t.compLex==SB_SEP)
        {
            Match(t.compLex,SB_SEP);
            ParsearListaArgumentos();
        }
    }
}

void ParsearSentenciaUser()
{
    Match(t.compLex,ID_USER);
    // Test for function call
    if (t.compLex==SB_PRAB)
    {
        Match(t.compLex,SB_PRAB);
        ParsearListaArgumentos();
        Match(t.compLex,SB_PRCR);
    }
    else
    {
        // Parsear Assignment
        if (t.compLex==SB_CHAB)
        {
            Match(t.compLex,SB_CHAB);
            ParsearExpr();
            Match(t.compLex,SB_CHCR);
        }
        Match(t.compLex,OP_ASIG);
        ParsearExpr();
    }
}

void ParsearSentencia()
{
  if (t.compLex==SB_IF)
        ParsearSentenciaIf();
    else if (t.compLex==SB_FOR)
        ParsearSentenciaFor();
    else
        ParsearSentenciaUser();
}

void ParsearBloqueSenctencia()
{
    while (t.compLex!=EOF &&
           t.compLex!=SB_END &&
           t.compLex!=SB_ELSE)
    {
        if (t.compLex!=SB_SPLN)
            ParsearSentencia();
        Match(t.compLex,SB_SPLN);
    }
}

void ParsearTamArreglo()
{
    Match(t.compLex,SB_CHAB);
    Match(t.compLex,NUM_USER);
    Match(t.compLex,SB_CHCR);
}

void ParsearVariablesDeclaracion()
{
    Match(t.compLex,ID_USER);
    if (t.compLex==SB_CHAB)
    {
        ParsearTamArreglo();
    }
}

void ParsearListaDeclaracion()
{
    ParsearVariablesDeclaracion();
    if (t.compLex==SB_SEP)
    {
        Match(t.compLex,SB_SEP);
        ParsearListaDeclaracion();
    }
}

void ParsearDeclaracion()
{
    Match(t.compLex,SB_VAR );
    ParsearListaDeclaracion();
    Match(t.compLex,SB_SPLN);
    if (t.compLex==SB_VAR)
        ParsearDeclaracion();
}

void ParsearInstruccion()
{
    if (t.compLex==SB_VAR)
        ParsearDeclaracion();
    ParsearBloqueSenctencia(); // NULLABLE
}

void ParsearPrograma()
{
    ParsearInstruccion();
}

void ParsearFuente()
{
    sigLex();
    if (t.compLex!=EOF)
        ParsearPrograma();
}

int main(int argc,char* args[])
{
	// inicializar analizador lexico
	int complex=0;
	initTabla();
	initTablaSimbolos();
	
	if(argc > 1)
	{
		if (!(archivo=fopen(args[1],"rt")))
		{
			printf("Archivo no encontrado.\n");
			exit(1);
		}
		ParsearFuente();
		fclose(archivo);
	}else{
		printf("Debe pasar como parametro el path al archivo fuente.\n");
		exit(1);
	}
        
        printf("Analisis finalizado..\n");

	return 0;
}

 
