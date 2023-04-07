package MiniC.Parser;


import MiniC.Scanner.Token;
import MiniC.Scanner.SourcePos;
import MiniC.Parser.SyntaxError;
import MiniC.Scanner.Scanner;
import MiniC.ErrorReporter;

public class Parser {

  private Scanner scanner;
  private ErrorReporter errorReporter;
  private Token currentToken;

  public Parser(Scanner lexer, ErrorReporter reporter) {
    scanner = lexer;
    errorReporter = reporter;
  }

  // accept() checks whether the current token matches tokenExpected.
  // If so, it fetches the next token.
  // If not, it reports a syntax error.
  void accept (int tokenExpected) throws SyntaxError {
    if (currentToken.kind == tokenExpected) {
      currentToken = scanner.scan();
    } else {
      syntaxError("\"%\" expected here", Token.spell(tokenExpected));
    }
  }

  // acceptIt() unconditionally accepts the current token
  // and fetches the next token from the scanner.
  void acceptIt() {
    currentToken = scanner.scan();
  }

  void syntaxError(String messageTemplate, String tokenQuoted) throws SyntaxError {
    SourcePos pos = currentToken.GetSourcePos();
    errorReporter.reportError(messageTemplate, tokenQuoted, pos);
    throw(new SyntaxError());
  }

  boolean isTypeSpecifier(int token) {
    if (token == Token.VOID ||
      token == Token.INT  ||
      token == Token.BOOL ||
      token == Token.FLOAT) {
      return true;
    } else {
      return false;
    }
  }

  ///////////////////////////////////////////////////////////////////////////////
  //
  // toplevel parse() routine:
  //
  ///////////////////////////////////////////////////////////////////////////////

  public void parse() {

    currentToken = scanner.scan(); // get first token from scanner...

    try {
      parseProgram();
      if (currentToken.kind != Token.EOF) {
        syntaxError("\"%\" not expected after end of program",
            currentToken.GetLexeme());
      }
    }
    catch (SyntaxError s) {return; /* to be refined in Assignment 3...*/ }
    return;
  }


  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseProgram():
  //
  // program ::= ( (VOID|INT|BOOL|FLOAT) ID ( FunPart | VarPart ) )*
  //
  ///////////////////////////////////////////////////////////////////////////////

  public void parseProgram() throws SyntaxError {
    while (isTypeSpecifier(currentToken.kind)) {
      acceptIt();
      accept(Token.ID);
      if(currentToken.kind == Token.LEFTPAREN) {
        parseFunPart();
      } else {
        parseVarPart();
      }
    }
  }


  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseFunPart():
  //
  // FunPart ::= ( "(" ParamsList? ")" CompoundStmt )
  //
  ///////////////////////////////////////////////////////////////////////////////

  public void parseFunPart() throws SyntaxError {
    // We already know that the current token is "(".
    // Otherwise use accept() !
    acceptIt();
    if (isTypeSpecifier(currentToken.kind)) {
      parseParamsList();
    }
    accept(Token.RIGHTPAREN);
    parseCompoundStmt();
  }


  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseParamsList():
  //
  // ParamsList ::= ParamsDecl ( "," ParamsDecl ) *
  //
  ///////////////////////////////////////////////////////////////////////////////

  public void parseParamsList() throws SyntaxError {
    acceptIt();
    parseDeclarator();
    while(currentToken.kind == Token.COMMA){
      acceptIt();
      if (isTypeSpecifier(currentToken.kind)) {
        acceptIt();
        parseDeclarator();
      }
      else{
        syntaxError("TypeSpecifier is expected in ParamsList",
            currentToken.GetLexeme());
      }
      
    }

  } 


  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseCompoundStmt():
  //
  // CompoundStmt ::= "{" VariableDefinition* Stmt* "}"
  //
  ///////////////////////////////////////////////////////////////////////////////

  public void parseCompoundStmt() throws SyntaxError {
    accept(Token.LEFTBRACE);
    while (isTypeSpecifier(currentToken.kind)){
      acceptIt();
      parseVariableDef();
    }
    while (currentToken.kind == Token.LEFTBRACE || currentToken.kind == Token.IF || currentToken.kind == Token.WHILE || currentToken.kind == Token.FOR || currentToken.kind == Token.RETURN || currentToken.kind == Token.ID){
      parseStmt();
    }
    accept(currentToken.RIGHTBRACE);

  }

  public void parseVariableDef() throws SyntaxError {
    parseInitDeclList();
    accept(Token.SEMICOLON);
  }

  public void parseStmt() throws SyntaxError {
    if(currentToken.kind == Token.LEFTBRACE){
      parseCompoundStmt();
    }

    else if(currentToken.kind == Token.IF){
      parseIfStmt();
    }

    else if(currentToken.kind == Token.WHILE){
      parseWhileStmt();
    }

    else if(currentToken.kind == Token.FOR){
      parseForStmt();
    }

    else if(currentToken.kind == Token.RETURN){
      acceptIt();
      if(currentToken.kind == Token.ID || currentToken.kind == Token.LEFTPAREN || currentToken.kind == Token.INTLITERAL || currentToken.kind == Token.BOOLLITERAL || currentToken.kind == Token.FLOATLITERAL || currentToken.kind == Token.STRINGLITERAL || currentToken.kind == Token.PLUS || currentToken.kind == Token.MINUS  || currentToken.kind == Token.NOT ){
        parseExpr();
      }
      accept(Token.SEMICOLON);
    }

    else if(currentToken.kind == Token.ID){
      acceptIt();

      if(currentToken.kind == Token.ASSIGN)
      {
        acceptIt();
        parseExpr();
        accept(Token.SEMICOLON);
      }

      else if(currentToken.kind == Token.LEFTBRACKET){
        acceptIt();
        parseExpr();
        accept(Token.RIGHTBRACKET);
        accept(Token.ASSIGN);
        parseExpr();
        accept(Token.SEMICOLON);
      }

      else if(currentToken.kind == Token.LEFTPAREN){
        parseArgList();
        accept(Token.SEMICOLON);
      }
    }
    else{
      syntaxError("\"%\" not expected in parseStmt",
            currentToken.GetLexeme());
    }
  }

  public void parseArgList() throws SyntaxError {
    acceptIt();
    if(currentToken.kind == Token.ID || currentToken.kind == Token.LEFTPAREN || currentToken.kind == Token.INTLITERAL || currentToken.kind == Token.BOOLLITERAL || currentToken.kind == Token.FLOATLITERAL || currentToken.kind == Token.STRINGLITERAL || currentToken.kind == Token.PLUS || currentToken.kind == Token.MINUS  || currentToken.kind == Token.NOT ){
      parseExpr();
      while(currentToken.kind == Token.COMMA){
        acceptIt();
        parseExpr();
      }
    }
    accept(Token.RIGHTPAREN);
  }

  public void parseIfStmt() throws SyntaxError {
    accept(Token.IF);
    accept(Token.LEFTPAREN);
    parseExpr();
    accept(Token.RIGHTPAREN);
    parseStmt();
    if(currentToken.kind == Token.ELSE){
      acceptIt();
      parseStmt();
    }
  }

  public void parseForStmt() throws SyntaxError {
    accept(Token.FOR);
    accept(Token.LEFTPAREN);
    if(currentToken.kind == Token.ID){
      parseAsgnExpr();
    }
    accept(Token.SEMICOLON);
    if(currentToken.kind == Token.ID || currentToken.kind == Token.LEFTPAREN || currentToken.kind == Token.INTLITERAL || currentToken.kind == Token.BOOLLITERAL || currentToken.kind == Token.FLOATLITERAL || currentToken.kind == Token.STRINGLITERAL || currentToken.kind == Token.PLUS || currentToken.kind == Token.MINUS  || currentToken.kind == Token.NOT ){
      parseExpr();
    }
    accept(Token.SEMICOLON);
    if(currentToken.kind == Token.ID){
      parseAsgnExpr();
    }
    accept(Token.RIGHTPAREN);
    parseStmt();
  }

  public void parseAsgnExpr() throws SyntaxError {
    accept(Token.ID);
    accept(Token.ASSIGN);
    parseExpr();
  }

  public void parseWhileStmt() throws SyntaxError {
    accept(Token.WHILE);
    accept(Token.LEFTPAREN);
    parseExpr();
    accept(Token.RIGHTPAREN);
    parseStmt();
  }

  public void parseExpr() throws SyntaxError {
    parseAndExpr();
    while(currentToken.kind == Token.OR){
      acceptIt();
      parseAndExpr();
    }
  }

  public void parseAndExpr() throws SyntaxError {
    parseRelationalExpr();
    while(currentToken.kind == Token.AND){
      acceptIt();
      parseRelationalExpr();
    }
  }

  public void parseRelationalExpr() throws SyntaxError {
    parseAddExpr();
    if(currentToken.kind == Token.EQ || currentToken.kind == Token.NOTEQ || currentToken.kind == Token.LESS || currentToken.kind == Token.LESSEQ || currentToken.kind == Token.GREATER || currentToken.kind == Token.GREATEREQ){
      acceptIt();
      parseAddExpr();
    }
  }

  public void parseAddExpr() throws SyntaxError {
    parseMultExpr();
    while(currentToken.kind == Token.PLUS || currentToken.kind == Token.MINUS){
      acceptIt();
      parseMultExpr();
    }
  }

  public void parseMultExpr() throws SyntaxError {
    parseUnaryExpr();
    while(currentToken.kind == Token.DIV || currentToken.kind == Token.TIMES){
      acceptIt();
      parseUnaryExpr();
    }
  }

  public void parseUnaryExpr() throws SyntaxError {
    if(currentToken.kind == Token.ID || currentToken.kind == Token.LEFTPAREN || currentToken.kind == Token.INTLITERAL || currentToken.kind == Token.BOOLLITERAL || currentToken.kind == Token.FLOATLITERAL || currentToken.kind == Token.STRINGLITERAL){
      parsePrimaryExpr();
    }

    else if(currentToken.kind == Token.PLUS || currentToken.kind == Token.MINUS  || currentToken.kind == Token.NOT ){
      acceptIt();
      parseUnaryExpr();
    }

    else{
      syntaxError("\"%\" not expected in UnaryExpr",
      currentToken.GetLexeme());
    }
  }

  public void parsePrimaryExpr() throws SyntaxError {

    if(currentToken.kind == Token.ID){
      acceptIt();
      if(currentToken.kind == Token.LEFTPAREN){
        parseArgList();
      }

      else if(currentToken.kind == Token.LEFTBRACKET){
        accept(Token.LEFTBRACKET);
        parseExpr();
        accept(Token.RIGHTBRACKET);
      }
    }

    else if(currentToken.kind == Token.LEFTPAREN){
      acceptIt();
      parseExpr();
      accept(Token.RIGHTPAREN);
    }

    else if(currentToken.kind == Token.INTLITERAL){
      acceptIt();
    }

    else if(currentToken.kind == Token.BOOLLITERAL){
      acceptIt();
    }

    else if(currentToken.kind == Token.FLOATLITERAL){
      acceptIt();
    }

    else if(currentToken.kind == Token.STRINGLITERAL){
      acceptIt();
    }

    else{
      syntaxError("\"%\" not expected in PrimaryExpr",
          currentToken.GetLexeme());
    }
  }
  
  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseVarPart():
  //
  // VarPart ::= ( "[" INTLITERAL "]" )?  ( "=" initializer ) ? ( "," init_decl)* ";"
  //
  ///////////////////////////////////////////////////////////////////////////////

  public void parseVarPart() throws SyntaxError {
    if(currentToken.kind == Token.LEFTBRACKET){
      accept(Token.LEFTBRACKET);
      accept(Token.INTLITERAL);
      accept(Token.RIGHTBRACKET);
    }

    if(currentToken.kind == Token.ASSIGN){
      accept(Token.ASSIGN);
      parseInitializer();
    }

    while(currentToken.kind == Token.COMMA){
      accept(Token.COMMA);
      parseInitDecl();
    }

    accept(Token.SEMICOLON);

  }


  public void parseInitializer() throws SyntaxError {
    if(currentToken.kind == Token.ID || currentToken.kind == Token.LEFTPAREN || currentToken.kind == Token.INTLITERAL || currentToken.kind == Token.BOOLLITERAL || currentToken.kind == Token.FLOATLITERAL || currentToken.kind == Token.STRINGLITERAL || currentToken.kind == Token.PLUS || currentToken.kind == Token.MINUS  || currentToken.kind == Token.NOT ){
      parseExpr();
    }
    else if(currentToken.kind == Token.LEFTBRACE){
      acceptIt();
      parseExpr();
      while(currentToken.kind == Token.COMMA){
        acceptIt();
        parseExpr();
      }
      accept(Token.RIGHTBRACE);
    }
    else{
      syntaxError("\"%\" not expected in parseInitializer",
            currentToken.GetLexeme());
    }
  }

  public void parseInitDeclList() throws SyntaxError {
    parseInitDecl();
    while(currentToken.kind == Token.COMMA){
      acceptIt();
      parseInitDecl();
    }
  }

  public void parseInitDecl() throws SyntaxError {
    parseDeclarator();
    if(currentToken.kind == Token.ASSIGN){
      acceptIt();
      parseInitializer();
    }
  }

  public void parseDeclarator() throws SyntaxError {
    accept(Token.ID);
    if(currentToken.kind == Token.LEFTBRACKET){
      acceptIt();
      accept(Token.INTLITERAL);
      accept(Token.RIGHTBRACKET);
    }
  }
}
