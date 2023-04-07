package MiniC.Parser;



import MiniC.Scanner.Scanner;
import MiniC.Scanner.Token;
import MiniC.Scanner.SourcePos;
import MiniC.Parser.SyntaxError;
import MiniC.ErrorReporter;
import MiniC.AstGen.*;


public class Parser {

  private Scanner scanner;
  private ErrorReporter errorReporter;
  private Token currentToken;
  private SourcePos previousTokenPosition;

  public Parser(Scanner lexer, ErrorReporter reporter) {
    scanner = lexer;
    errorReporter = reporter;
  }

  // accept() checks whether the current token matches tokenExpected.
  // If so, it fetches the next token.
  // If not, it reports a syntax error.
  void accept (int tokenExpected) throws SyntaxError {
    if (currentToken.kind == tokenExpected) {
      previousTokenPosition = currentToken.GetSourcePos();
      currentToken = scanner.scan();
    } else {
      syntaxError("\"%\" expected here", Token.spell(tokenExpected));
    }
  }

  // acceptIt() unconditionally accepts the current token
  // and fetches the next token from the scanner.
  void acceptIt() {
    previousTokenPosition = currentToken.GetSourcePos();
    currentToken = scanner.scan();
  }

  // start records the position of the start of a phrase.
  // This is defined to be the position of the first
  // character of the first token of the phrase.
  void start(SourcePos pos) {
    pos.StartCol = currentToken.GetSourcePos().StartCol;
    pos.StartLine = currentToken.GetSourcePos().StartLine;
  }

  // finish records the position of the end of a phrase.
  // This is defined to be the position of the last
  // character of the last token of the phrase.
  void finish(SourcePos pos) {
    pos.EndCol = previousTokenPosition.EndCol;
    pos.EndLine = previousTokenPosition.EndLine;
  }

  void syntaxError(String messageTemplate, String tokenQuoted) throws SyntaxError {
    SourcePos pos = currentToken.GetSourcePos();
    errorReporter.reportError(messageTemplate, tokenQuoted, pos);
    throw(new SyntaxError());
  }

  boolean isTypeSpecifier(int token) {
    if(token == Token.VOID ||
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
  // parseArrayIndexDecl (Type T):
  //
  // Take [INTLITERAL] and generate an ArrayType
  //
  ///////////////////////////////////////////////////////////////////////////////

  public ArrayType parseArrayIndexDecl(Type T) throws SyntaxError {
    IntLiteral L;
    IntExpr IE;
    accept(Token.LEFTBRACKET);
    SourcePos pos = currentToken.GetSourcePos();
    L = new IntLiteral(currentToken.GetLexeme(), pos);
    accept(Token.INTLITERAL);
    accept(Token.RIGHTBRACKET);
    IE = new IntExpr (L, pos);
    return new ArrayType (T, IE, previousTokenPosition);
  }


  ///////////////////////////////////////////////////////////////////////////////
  //
  // toplevel parse() routine:
  //
  ///////////////////////////////////////////////////////////////////////////////

  public Program parse() {

    Program ProgramAST = null;

    previousTokenPosition = new SourcePos();
    previousTokenPosition.StartLine = 0;
    previousTokenPosition.StartCol = 0;
    previousTokenPosition.EndLine = 0;
    previousTokenPosition.EndCol = 0;

    currentToken = scanner.scan(); // get first token from scanner...

    try {
      ProgramAST = parseProgram();
      if (currentToken.kind != Token.EOF) {
        syntaxError("\"%\" not expected after end of program",
            currentToken.GetLexeme());
      }
    }
    catch (SyntaxError s) { return null; }
    return ProgramAST;
  }


  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseProgram():
  //
  // program ::= ( (VOID|INT|BOOL|FLOAT) ID ( FunPart | VarPart ) )* ";"
  //
  ///////////////////////////////////////////////////////////////////////////////

  // parseProgDecls: recursive helper function to facilitate AST construction.
  public Decl parseProgDecls () throws SyntaxError {
    if (! isTypeSpecifier(currentToken.kind)) {
      return new EmptyDecl (previousTokenPosition);
    }
    SourcePos pos = new SourcePos();
    start(pos);
    Type T = parseTypeSpecifier();
    ID Ident = parseID();
    if(currentToken.kind == Token.LEFTPAREN) {
      Decl newD = parseFunPart(T, Ident, pos);
      return new DeclSequence (newD, parseProgDecls(), previousTokenPosition);
    } else {
      DeclSequence Vars = parseVarPart(T, Ident);
      DeclSequence VarsTail = Vars.GetRightmostDeclSequenceNode();
      Decl RemainderDecls = parseProgDecls();
      VarsTail.SetRightSubtree (RemainderDecls);
      return Vars;
    }
  }

  public Program parseProgram() throws SyntaxError {
    SourcePos pos = new SourcePos();
    start(pos);
    Decl D = parseProgDecls();
    finish(pos);
    Program P = new Program (D, pos);
    return P;
  }


  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseFunPart():
  //
  // FunPart ::= ( "(" ParamsList? ")" CompoundStmt )
  //
  ///////////////////////////////////////////////////////////////////////////////

  public Decl parseFunPart(Type T, ID Ident, SourcePos pos) throws SyntaxError {

    // We already know that the current token is "(".
    // Otherwise use accept() !
    acceptIt();
    Decl PDecl = parseParamsList(); // can also be empty...
    accept(Token.RIGHTPAREN);
    CompoundStmt CStmt = parseCompoundStmt();
    finish(pos);
    return new FunDecl (T, Ident, PDecl, CStmt, pos);
  }


  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseParamsList():
  //
  // ParamsList ::= ParameterDecl ( "," ParameterDecl ) *
  //
  ///////////////////////////////////////////////////////////////////////////////

  public Decl parseParamsList() throws SyntaxError {
    if (!isTypeSpecifier(currentToken.kind)) {
      return new EmptyFormalParamDecl(previousTokenPosition);
    }
    Decl Decl_1 = parseParameterDecl();
    Decl Decl_r = new EmptyFormalParamDecl(previousTokenPosition);
    if (currentToken.kind == Token.COMMA) {
      acceptIt();
      Decl_r = parseParamsList();
      if (Decl_r instanceof EmptyFormalParamDecl) {
        syntaxError("Declaration after comma expected", "");
      }
    }
    return new FormalParamDeclSequence (Decl_1, Decl_r, previousTokenPosition);
  } 


  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseParameterDecl():
  //
  // ParameterDecl ::= (VOID|INT|BOOL|FLOAT) Declarator
  //
  ///////////////////////////////////////////////////////////////////////////////

  public Decl parseParameterDecl() throws SyntaxError {
    Type T = null;
    Decl D = null;

    SourcePos pos = new SourcePos();
    start(pos);
    if (isTypeSpecifier(currentToken.kind)) {
      T = parseTypeSpecifier();
    } else {
      syntaxError("Type specifier instead of % expected",
          Token.spell(currentToken.kind));
    }
    D = parseDeclarator(T, pos);
    return D;
  }


  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseDeclarator():
  //
  // Declarator ::= ID ( "[" INTLITERAL "]" )?
  //
  ///////////////////////////////////////////////////////////////////////////////

  public Decl parseDeclarator(Type T, SourcePos pos) throws SyntaxError {
    ID Ident = parseID();
    if (currentToken.kind == Token.LEFTBRACKET) {
      ArrayType ArrT = parseArrayIndexDecl(T);
      finish(pos);
      return new FormalParamDecl (ArrT, Ident, pos);
    }
    finish(pos);
    return new FormalParamDecl (T, Ident, pos);
  }


  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseVarPart():
  //
  // VarPart ::= ( "[" INTLITERAL "]" )?  ( "=" initializer ) ? ( "," init_decl)* ";"
  //
  ///////////////////////////////////////////////////////////////////////////////
  public DeclSequence parseInitDecl(Type T) throws SyntaxError {
    Type theType = T;
    ID Ident = parseID();
    Decl D;
    DeclSequence Seq = null;
    Expr E = new EmptyExpr(previousTokenPosition);
    if (currentToken.kind == Token.LEFTBRACKET) {
      theType = parseArrayIndexDecl(T);
    }
    if(currentToken.kind == Token.ASSIGN){
      acceptIt();
      E = parseInitializer();
    }
    D = new VarDecl (theType, Ident, E, previousTokenPosition);
    
    if (currentToken.kind == Token.COMMA) {
      acceptIt();
      Seq = new DeclSequence (D, parseInitDecl(T), previousTokenPosition);
    }
    else {
      Seq = new DeclSequence (D, new EmptyDecl (previousTokenPosition),
      previousTokenPosition);
    }

    return Seq;

  }

  public Expr parseInitializer() throws SyntaxError{
    Expr expr = null;
    if(currentToken.kind == Token.ID || currentToken.kind == Token.LEFTPAREN || currentToken.kind == Token.INTLITERAL || currentToken.kind == Token.BOOLLITERAL || currentToken.kind == Token.FLOATLITERAL || currentToken.kind == Token.STRINGLITERAL || currentToken.kind == Token.PLUS || currentToken.kind == Token.MINUS  || currentToken.kind == Token.NOT ){
      expr = parseExpr();
      return expr;
    }
    
    if(currentToken.kind == Token.LEFTBRACE){
      acceptIt();
      expr = parseExpr();
      Expr exprSeq = new ExprSequence(expr, parseInitializer(), previousTokenPosition);
      accept(Token.RIGHTBRACE);
      return exprSeq;
    }

    if(currentToken.kind == Token.COMMA){
      acceptIt();
      expr = parseExpr();
      Expr exprSeq = new ExprSequence(expr, parseInitializer(), previousTokenPosition);
      return exprSeq;
    }
      
    else{
      expr = new EmptyExpr(previousTokenPosition);
    }

    return expr;
  }

  public DeclSequence parseVarPart(Type T, ID Ident) throws SyntaxError {
    Type theType = T;
    Decl D;
    DeclSequence Seq = null;
    Expr E = new EmptyExpr(previousTokenPosition);
    if (currentToken.kind == Token.LEFTBRACKET) {
      theType = parseArrayIndexDecl(T);
    }
    if (currentToken.kind == Token.ASSIGN) {
      acceptIt();
      // You can use the following code after you have implemented
      // parseInitializer():
      E = parseInitializer();
    }
    D = new VarDecl (theType, Ident, E, previousTokenPosition);
    // You can use the following code after implementatin of parseInitDecl():
    
    if (currentToken.kind == Token.COMMA) {
      acceptIt();
      Seq = new DeclSequence (D, parseInitDecl(T), previousTokenPosition);
    }
    else {
      Seq = new DeclSequence (D, new EmptyDecl (previousTokenPosition),
      previousTokenPosition);
    }
     
    accept (Token.SEMICOLON);
    return Seq;
  }


  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseUnaryExpr():
  //
  // UnaryExpr ::= ("+"|"-"|"!")* PrimaryExpr
  //
  ///////////////////////////////////////////////////////////////////////////////

  public Stmt parseIfStmt() throws SyntaxError {
    Expr eAST;
    Stmt thenAST, elseAST;
    
    acceptIt();
    accept(Token.LEFTPAREN);
    eAST = parseExpr();
    accept(Token.RIGHTPAREN);
    thenAST = parseStmt();

    if(currentToken.kind == Token.ELSE) {
      acceptIt();
      elseAST = parseStmt();
      return new IfStmt(eAST,thenAST,elseAST,previousTokenPosition);
    }

    else{
      return new IfStmt(eAST,thenAST,previousTokenPosition);
    }
  }

  public Stmt parseWhileStmt() throws SyntaxError {
    Expr eAST;
    Stmt stmtAST;
    
    acceptIt();
    accept(Token.LEFTPAREN);
    eAST = parseExpr();
    accept(Token.RIGHTPAREN);
    stmtAST = parseStmt();

    return new WhileStmt(eAST,stmtAST, previousTokenPosition);
  }

  public Stmt parseForStmt() throws SyntaxError {
    Expr e1AST, e2AST, e3AST;
    Stmt stmtAST;
    SourcePos pos = new SourcePos();
    acceptIt();

    accept(Token.LEFTPAREN);

    start(pos);
    if(currentToken.kind == Token.ID){
      ID iden = parseID();
      VarExpr varExpr = new VarExpr(iden, previousTokenPosition);
      accept(Token.ASSIGN);
      Expr r1AST = parseExpr();
      finish(pos);
      e1AST = new AssignExpr(varExpr,r1AST,pos);
    }
    
    else{
      e1AST = new EmptyExpr(previousTokenPosition);
    }

    accept(Token.SEMICOLON);
    if(currentToken.kind == Token.SEMICOLON){
      e2AST = new EmptyExpr(previousTokenPosition);
    }
    else{
      e2AST = parseExpr();
    }
    accept(Token.SEMICOLON);

    start(pos);
    if(currentToken.kind == Token.ID){
      ID iden2 = parseID();
      VarExpr varExpr2 = new VarExpr(iden2, previousTokenPosition);
      accept(Token.ASSIGN);
      Expr r2AST = parseExpr();
      finish(pos);
      e3AST = new AssignExpr(varExpr2,r2AST,pos);
    }
    
    else{
      e3AST = new EmptyExpr(previousTokenPosition);
    }

    accept(Token.RIGHTPAREN);

    stmtAST = parseStmt();

    return new ForStmt(e1AST,e2AST,e3AST,stmtAST,previousTokenPosition);
  }

  public Stmt parseStmt() throws SyntaxError {
    Stmt stmt = null;
    Expr expr = null;
    if(currentToken.kind == Token.LEFTBRACE){
      stmt = parseCompoundStmt();
    }

    else if(currentToken.kind == Token.IF){
      stmt = parseIfStmt();
    }

    else if(currentToken.kind == Token.WHILE){
      stmt = parseWhileStmt();
    }

    else if(currentToken.kind == Token.FOR){
      stmt = parseForStmt();
    }

    else if(currentToken.kind == Token.RETURN){
      SourcePos pos = new SourcePos();
      start(pos);
      acceptIt();
      if(currentToken.kind != Token.SEMICOLON){
        expr = parseExpr();
      }
      else{
        expr = new EmptyExpr(previousTokenPosition);
      }
      accept(Token.SEMICOLON);
      finish(pos);
      stmt = new ReturnStmt(expr, pos);
    }

    else if(currentToken.kind == Token.ID){
      SourcePos pos = new SourcePos();
      start(pos);

      ID iden = parseID();
      VarExpr varExpr = new VarExpr(iden, previousTokenPosition);
      

      if(currentToken.kind == Token.ASSIGN)
      {
        acceptIt();
        expr = parseExpr();
        accept(Token.SEMICOLON);
        finish(pos);
        stmt = new AssignStmt(varExpr, expr, pos);
      }

      else if(currentToken.kind == Token.LEFTBRACKET){
        acceptIt();
        expr = parseExpr();
        accept(Token.RIGHTBRACKET);
        finish(pos);
        ArrayExpr arrayExpr = new ArrayExpr(varExpr, expr, pos);
        

        accept(Token.ASSIGN);
        
        Expr rightExpr = parseExpr();
        accept(Token.SEMICOLON);
        finish(pos);
        stmt = new AssignStmt(arrayExpr, rightExpr, pos);
      }

      else if(currentToken.kind == Token.LEFTPAREN){
        Expr param = parseArgList();
        accept(Token.SEMICOLON);
        finish(pos);
        CallExpr callExpr = new CallExpr(iden, param, pos);
        stmt = new CallStmt(callExpr, pos);
      }
    }
    else{
      syntaxError("\"%\" not expected in parseStmt",
            currentToken.GetLexeme());
      stmt = new EmptyStmt(previousTokenPosition);
    }
    return stmt;
  }

  public Expr parseExpr() throws SyntaxError {

    Expr retExpr = parseAndExpr();
    while(currentToken.kind == Token.OR){
      Operator opAST = new Operator (currentToken.GetLexeme(),
          previousTokenPosition);
      acceptIt();
      Expr secondExpr = parseAndExpr();
      retExpr = new BinaryExpr(retExpr, opAST, secondExpr, previousTokenPosition);
    }

    return retExpr;
  }

  public Expr parseAndExpr() throws SyntaxError {
    Expr retExpr = parseRelationalExpr();
    while(currentToken.kind == Token.AND){
      Operator opAST = new Operator (currentToken.GetLexeme(),
          previousTokenPosition);
      acceptIt();
      Expr secondExpr = parseRelationalExpr();
      retExpr = new BinaryExpr(retExpr, opAST, secondExpr, previousTokenPosition);
    }

    return retExpr;
  }

  public Expr parseRelationalExpr() throws SyntaxError {
    Expr firstExpr = parseAddExpr();
    if(currentToken.kind == Token.EQ || currentToken.kind == Token.NOTEQ || currentToken.kind == Token.LESS || currentToken.kind == Token.LESSEQ || currentToken.kind == Token.GREATER || currentToken.kind == Token.GREATEREQ){
      Operator opAST = new Operator (currentToken.GetLexeme(),
          previousTokenPosition);
      acceptIt();
      return new BinaryExpr(firstExpr, opAST,  parseAddExpr(), previousTokenPosition);
    }
    return firstExpr;
  }

  public Expr parseAddExpr() throws SyntaxError {
    Expr retExpr = parseMultExpr();
    while(currentToken.kind == Token.PLUS || currentToken.kind == Token.MINUS){
      Operator opAST = new Operator (currentToken.GetLexeme(),
          previousTokenPosition);
      acceptIt();
      Expr secondExpr = parseMultExpr();
      retExpr = new BinaryExpr(retExpr, opAST, secondExpr, previousTokenPosition);
    }

    return retExpr;
  }

  public Expr parseMultExpr() throws SyntaxError {
    Expr retExpr = parseUnaryExpr();
    while(currentToken.kind == Token.DIV || currentToken.kind == Token.TIMES){
      Operator opAST = new Operator (currentToken.GetLexeme(),
          previousTokenPosition);
      acceptIt();
      Expr secondExpr = parseUnaryExpr();
      retExpr = new BinaryExpr(retExpr, opAST, secondExpr, previousTokenPosition);
    }

    return retExpr;
  }

  public Expr parseUnaryExpr() throws SyntaxError {
    if (currentToken.kind == Token.PLUS ||
        currentToken.kind == Token.MINUS ||
        currentToken.kind == Token.NOT) {
      Operator opAST = new Operator (currentToken.GetLexeme(),
          previousTokenPosition);
      acceptIt();
      return new UnaryExpr (opAST, parseUnaryExpr(), previousTokenPosition);
    }

    else if(currentToken.kind == Token.ID || currentToken.kind == Token.LEFTPAREN || currentToken.kind == Token.INTLITERAL || currentToken.kind == Token.BOOLLITERAL || currentToken.kind == Token.FLOATLITERAL || currentToken.kind == Token.STRINGLITERAL){
      return parsePrimaryExpr();
    }

    else{
      syntaxError("\"%\" not expected in UnaryExpr",
      currentToken.GetLexeme());
      return new EmptyExpr(previousTokenPosition);
    }
  }


  ///////////////////////////////////////////////////////////////////////////////
  //
  // parsePrimaryExpr():
  //
  // PrimaryExpr ::= ID arglist?
  //              |  ID "[" expr "]"
  //              |  "(" expr ")"
  //              |  INTLITERAL | BOOLLITERAL | FLOATLITERAL | STRINGLITERAL
  //
  ///////////////////////////////////////////////////////////////////////////////

  public Expr parsePrimaryExpr() throws SyntaxError {
    Expr retExpr = null;
    VarExpr varExpr = null;
    SourcePos pos = new SourcePos();
    start(pos);
    if(currentToken.kind == Token.ID){
      ID Iden = parseID();
      retExpr = new VarExpr(Iden, previousTokenPosition);
      if(currentToken.kind == Token.LEFTPAREN){
        Expr param = parseArgList();
        finish(pos);
        CallExpr callExpr = new CallExpr(Iden, param, pos);
        return callExpr;
      }

      else if(currentToken.kind == Token.LEFTBRACKET){
        acceptIt();
        varExpr = new VarExpr(Iden, previousTokenPosition);
        Expr expr = parseExpr();
        accept(Token.RIGHTBRACKET);
        retExpr = new ArrayExpr(varExpr, expr, previousTokenPosition);
      }
    }

    else if(currentToken.kind == Token.LEFTPAREN){
      acceptIt();
      retExpr = parseExpr();
      accept(Token.RIGHTPAREN);
    }

    else if(currentToken.kind == Token.INTLITERAL){
      String string = currentToken.GetLexeme();
      acceptIt();
      IntLiteral intLiteral = new IntLiteral(string,previousTokenPosition);
      IntExpr intExpr = new IntExpr(intLiteral,previousTokenPosition);

      retExpr = intExpr;
    }

    else if(currentToken.kind == Token.BOOLLITERAL){
      String string = currentToken.GetLexeme();
      acceptIt();
      BoolLiteral boolLiteral = new BoolLiteral(string,previousTokenPosition);
      BoolExpr boolExpr = new BoolExpr(boolLiteral,previousTokenPosition);
      retExpr = boolExpr;
    }

    else if(currentToken.kind == Token.FLOATLITERAL){
      String string = currentToken.GetLexeme();
      acceptIt();
      FloatLiteral floatLiteral = new FloatLiteral(string,previousTokenPosition);
      FloatExpr floatExpr = new FloatExpr(floatLiteral,previousTokenPosition);
      retExpr = floatExpr;
    }

    else if(currentToken.kind == Token.STRINGLITERAL){
      String string = currentToken.GetLexeme();
      acceptIt();
      StringLiteral stringLiteral = new StringLiteral(string,previousTokenPosition);
      StringExpr stringExpr = new StringExpr(stringLiteral,previousTokenPosition);
      retExpr = stringExpr;
    }

    else{
      syntaxError("\"%\" not expected in PrimaryExpr",
          currentToken.GetLexeme());
      retExpr = new EmptyExpr(previousTokenPosition);
    }

    return retExpr;
  }


  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseCompoundStmt():
  //
  // CompoundStmt ::= "{" VariableDef* Stmt* "}"
  //
  ///////////////////////////////////////////////////////////////////////////////

  public Decl parseCompoundDecls () throws SyntaxError {
    if (!isTypeSpecifier(currentToken.kind)) {
      return new EmptyDecl (previousTokenPosition);
    }
    Type T = parseTypeSpecifier();
    ID Ident = parseID();
    DeclSequence Vars = parseVarPart(T, Ident);
    DeclSequence VarsTail = Vars.GetRightmostDeclSequenceNode();
    Decl RemainderDecls = parseCompoundDecls();
    VarsTail.SetRightSubtree (RemainderDecls);
    return Vars;       
  }

  public Stmt parseCompoundStmts () throws SyntaxError {
    if (! (currentToken.kind == Token.LEFTBRACE ||
          currentToken.kind == Token.IF ||
          currentToken.kind == Token.WHILE ||
          currentToken.kind == Token.FOR ||
          currentToken.kind == Token.RETURN ||
          currentToken.kind == Token.ID)
       ) {
      return new EmptyStmt(previousTokenPosition);
    }
    Stmt S = null;
    // You can use the following code after implementation of parseStmt():
    S = parseStmt();
    return new StmtSequence (S, parseCompoundStmts(), previousTokenPosition);
  }

  public CompoundStmt parseCompoundStmt() throws SyntaxError {
    SourcePos pos = new SourcePos();
    start(pos);
    accept(Token.LEFTBRACE);
    Decl D = parseCompoundDecls();
    Stmt S = parseCompoundStmts();
    accept(Token.RIGHTBRACE);
    finish(pos);
    if ( (D.getClass() == EmptyDecl.class) &&
        (S.getClass() == EmptyStmt.class)) {
      return new EmptyCompoundStmt (previousTokenPosition);
    } else {
      return new CompoundStmt (D, S, pos);
    }
  }


  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseArgList():
  //
  // ArgList ::= "(" ( arg ( "," arg )* )? ")"
  //
  ///////////////////////////////////////////////////////////////////////////////

  public Expr parseArgs() throws SyntaxError {
    if (currentToken.kind == Token.RIGHTPAREN) {
      return new  EmptyActualParam (previousTokenPosition);
    } 
    Expr Params = null;
    
     // You can use the following code after you have implemented parseExpr() aso.:

     Params = new ActualParam (parseExpr(), previousTokenPosition);
     if (currentToken.kind == Token.COMMA) {
     acceptIt();
     }
     
    return new ActualParamSequence (Params, parseArgs(), previousTokenPosition);
  }

  public Expr parseArgList() throws SyntaxError {
    accept(Token.LEFTPAREN);
    Expr Params = parseArgs();
    accept(Token.RIGHTPAREN);
    return Params;
  }


  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseID():
  //
  // ID (terminal)
  //
  ///////////////////////////////////////////////////////////////////////////////

  public ID parseID() throws SyntaxError {
    ID Ident = new ID(currentToken.GetLexeme(), currentToken.GetSourcePos());
    accept(Token.ID);
    return Ident;
  }


  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseTypeSpecifier():
  //
  // VOID | INT | FLOAT | BOOL (all terminals)
  //
  ///////////////////////////////////////////////////////////////////////////////

  public Type parseTypeSpecifier() throws SyntaxError {
    Type T = null;
    switch (currentToken.kind) {
      case Token.INT:
        T = new IntType(currentToken.GetSourcePos());
        break;
      case Token.FLOAT:
        T = new FloatType(currentToken.GetSourcePos());
        break;
      case Token.BOOL:
        T = new BoolType(currentToken.GetSourcePos());
        break;
      case Token.VOID:
        T = new VoidType(currentToken.GetSourcePos());
        break;
      default:
        syntaxError("Type specifier expected", "");
    }
    acceptIt();
    return T;
  }

}
