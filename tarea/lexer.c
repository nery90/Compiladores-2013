/*
 *	Analizador Léxico	
 *	Curso: Compiladores y Lenguajes de Bajo de Nivel
 *	Práctica de Programación Nro. 1
 *	Integrantes: - René Azcurra
 *                   - Nery Riquelme
 *	Descripcion:
 *	Implementa una analizador léxico para una calculadora de expresiones
 *      aritmeticas que reconoce números (enteros, reales, notación científica),
 *      operadores, y comentarios de un archivo fuente con lineas de operaciones aritméticas.
 *	
 * 	Despliega cada linea con los lexemas -> token
 *      y al final de la misma la expresión que representa línea
 * 
 *      Luego pasa la expresión a la función que se encarga de evaluar si la expresión es 
 *      correcta o no, retornando un mensaje.
 */

/*********** Librerias utilizadas **************/

#include<stdio.h>
#include<string.h>
#include<stdlib.h>
#include<ctype.h>

/***************** MACROS **********************/

//INPUTS
#define FACTOR		300

#define OPSUMA		256
#define OPRESTA		257
#define OPMULT		258
#define OPDIV		259
#define PARAB           260
#define PARCE           261

#define OPASIGNA	262

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
int mostrar=0;
int comment=0;
char expresion[TAMLEX*4];
/************** Prototipos *********************/


void sigLex();		// Del analizador Lexico

/**************** Funciones **********************/
int stricmp(const char* cad,const char* cad2) 
{
	int i;
	char c1[strlen(cad)];
	char c2[strlen(cad2)];
	
	strcpy(c1,cad);
	strcpy(c2,cad2);
	for(i=0;i<strlen(c1);i++)
		c1[i]=tolower(c1[i]);

	for(i=0;i<strlen(c2);i++)
		c2[i]=tolower(c2[i]);

	return strcmp(c1,c2);
}

/*********************HASH************************/
entrada *tabla;				//declarar la tabla de simbolos
int tamTabla=TAMHASH;		//utilizado para cuando se debe hacer rehash
int elems=0;				//utilizado para cuando se debe hacer rehash

int h(const char* k, int m)
{
	unsigned h=0,g;
	int i;
	for (i=0;i<strlen(k);i++)
	{
		h=(h << 4) + k[i];
		if (g=h&0xf0000000)
		{
			h=h^(g>>24);
			h=h^g;
		}
	}
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
	entrada pr,*e;
	insertTablaSimbolos("+",OPSUMA);
	insertTablaSimbolos("-",OPRESTA);
	insertTablaSimbolos("*",OPMULT);
	insertTablaSimbolos("/",OPDIV);
	insertTablaSimbolos("=",OPASIGNA);
        insertTablaSimbolos("(",PARAB);
        insertTablaSimbolos(")",PARCE);
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
	char aux=0;
	int acepto=0;
	int estado=0;
	char msg[41];
	entrada e;

	while((c=fgetc(archivo))!=EOF)
	{
		
		if (c==' ' || c=='\t')
                {
                    comment=1;
                    continue;	//eliminar espacios en blanco
                }
		else if(c=='\n')
		{
			numLinea++;//incrementar el numero de linea
			aux=c;
			c=fgetc(archivo);
			if (c==aux && comment==1)		
			{
				mostrar=0;
				comment=0;
			}
			else if (c!=aux && strlen(expresion)>0 && (comment==0 || isdigit(c)))
			{
				mostrar=1;
                                ungetc(c,archivo);
                                //aux=0;
			        break;
      			}
			ungetc(c,archivo);
                        aux=0;
			continue;
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
							e.compLex=FACTOR;
							insertar(e);
							t.pe=buscar(id);
						}
						t.compLex=FACTOR;
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
		else if (c=='+')
		{
			t.compLex=OPSUMA;
			t.pe=buscar("+");
			break;
		}
		else if (c=='-')
		{
			t.compLex=OPRESTA;
			t.pe=buscar("-");
			break;
		}
		else if (c=='*')
		{
			t.compLex=OPMULT;
			t.pe=buscar("*");
			break;
		}
		else if (c=='/')
		{
			if ((c=fgetc(archivo))=='/')
                        {//la secuencia es un comentario
				comment=1;
				while(c!=EOF && c!='\n')
                                {
                                        c=fgetc(archivo);					
                                }
                                ungetc(c,archivo);
                        }
                        else
                        {
				ungetc(c,archivo);
                                t.compLex=OPDIV;
                                t.pe=buscar("/");
                                break;
                        }
		}
                else if (c=='(')
		{
			t.compLex=PARAB;
			t.pe=buscar("(");
			break;
		}
                else if (c==')')
		{
			t.compLex=PARCE;
			t.pe=buscar(")");
			break;
		}
		else if (c=='=')
		{
			t.compLex=OPASIGNA;
			t.pe=buscar("=");
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

//Funciones principales con las operaciones básicas de la aritmética
//Que serán tenidas en cuenta
int op_suma(int a,int b)
{
   return (a+b);
}

int op_resta(int a,int b)
{
   return (a-b);
}

int op_division(int a,int b)
{
   return (a/b);
}

int op_multip(int a,int b)
{
   return (a*b);
}

//Estructura tipo de las funciones y sus correspondencias
struct s_operacion 
{
   char simbolo;
   int  prioridad;
   int  (*funcion)();
} ;

typedef	struct s_operacion t_operacion;

//Se almacena el tipo de operaciones que serán consideradas
t_operacion operacion[] =
{
    {'+',1,op_suma},
    {'-',1,op_resta},
    {'/',2,op_division},
    {'*',2,op_multip},
};

//Busca el último operador empezando del final de la cadena
int ultimo_op(char *str,int p)
{
   int posicion;
   int	i;
   int ret;
   int	par;
   
   ret = -2;
   par = 0;
   posicion = strlen(str)-1;
   while (posicion>=0)
   {
      if (str[posicion] == ')')
	 par ++;
      if (str[posicion] == '(')
	 par --;
      i = 0;
      while (!par && operacion[i].simbolo)
      {
	 if (operacion[i].simbolo==str[posicion])
	 {
	    ret = -1;
	    if (operacion[i].prioridad==p)
	       return (posicion);
	 }
	 i ++;
      }
      posicion --;
   }
   return (ret);
}

int es_nb(char *str)
{
  if (*str=='(')
    {
       str[strlen(str)-1] = 0;
       return (evaluar_expresion(str+1,0));
    }
  return (atoi(str));
}

//Descompone la expresión en sus factores y operadores
int evaluar_expresion(char *str,int p)
{
   int	pos;
   char	salvar_simbolo;
   int	nb2;
   
   if ((pos = ultimo_op(str,p))==-1)
      return (evaluar_expresion(str,p+1));
   if (pos==-2)
      return (es_nb(str));
   nb2 = evaluar_expresion(str+pos+1,p+1);
   salvar_simbolo = str[pos];
   str[pos] = 0;
   pos = 0;
   while (operacion[pos].simbolo)
   {
      if (salvar_simbolo==operacion[pos].simbolo)
	 return (operacion[pos].funcion(evaluar_expresion(str,p),nb2));
      pos ++;
   }   
   return (0);
}

int main(int argc,char* args[])
{
	// inicializar analizador lexico
	int complex=0;
	initTabla();
	initTablaSimbolos();
	char cadena[strlen(expresion)];
        
        if(argc > 1)
	{
		if (!(archivo=fopen(args[1],"rt")))
		{
			printf("Archivo no encontrado.\n");
			exit(1);
		}
		while (t.compLex!=EOF){
			sigLex();
			if (mostrar==1){
				printf("**********************************\n");
                                strcpy (cadena, expresion);
				printf("EXPRESION: %s = %d\n",expresion, evaluar_expresion(cadena, 0));
				printf("**********************************\n");
				memset(&expresion[0], 0, sizeof(expresion));
				mostrar=0;
			}
			else
                        {
				printf("Lin %d: %s -> %d\n",numLinea,t.pe->lexema,t.compLex);
              			strcat(expresion, t.pe->lexema);
                        }

			
		}
		fclose(archivo);
	}else{
		printf("Debe pasar como parametro el path al archivo fuente.\n");
		exit(1);
	}

	return 0;
}
