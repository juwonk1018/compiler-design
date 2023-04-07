package MiniC.Scanner;

import MiniC.Scanner.SourceFile;
import MiniC.Scanner.Token;

public final class Scanner {

  private SourceFile sourceFile;
  private SourceFile sourceFile_dup;

  private char currentChar;
  private boolean verbose;
  private StringBuffer currentLexeme;
  private boolean currentlyScanningToken;
  private int currentLineNr;
  private int currentColNr;

  private boolean isDigit(char c) {
    return (c >= '0' && c <= '9');
  }


///////////////////////////////////////////////////////////////////////////////
  static boolean Comments = false;
  static boolean starComments = false;
  static boolean prev1Flag = false;
  static boolean prev2Flag = false;
  static boolean madeToken = false;
  static int startColumn = 1;
  static int startLine = 1;

  Token prevToken1;
  Token prevToken2;
  Token current;
  public Scanner(SourceFile source) {
    sourceFile = source;
    currentChar = sourceFile.readChar();
    verbose = false;
    currentLineNr = 1;
    currentColNr= 1;
  }

  public void enableDebugging() {
    verbose = true;
  }

  // takeIt appends the current character to the current token, and gets
  // the next character from the source program (or the to-be-implemented
  // "untake" buffer in case of look-ahead characters that got 'pushed back'
  // into the input stream).

  private void takeIt() {
    if (currentlyScanningToken)
    {
      currentLexeme.append(currentChar);
    }
    currentChar = sourceFile.readChar();
  }

  private boolean isChar(char c) {
    return (((int)c >= 65 && (int)c <= 90) || ((int)c >= 97 && (int)c <= 122) ||c == '_');
  }

  private void exponent(char c) {
    currentColNr +=1;
    
    char temp = c;

    takeIt();

    char temp2 = ' ';
    if(currentChar =='+' || currentChar =='-'){
      temp2 = currentChar;
      currentColNr +=1;
      takeIt();
    }
    
    if(isDigit(currentChar)){
      currentColNr +=1;
      takeIt();
      while (isDigit(currentChar)) {
        currentColNr +=1;
        takeIt();
      }
    }
    else{
      if(temp2 != ' '){
        
        currentColNr-=2;
        currentLexeme.delete(currentLexeme.length()-2,currentLexeme.length());
        SourcePos posit_orig;
        posit_orig = new SourcePos();
        posit_orig.StartLine = currentLineNr;
        posit_orig.EndLine = currentLineNr;
        posit_orig.StartCol = startColumn;
        posit_orig.EndCol = currentColNr;
        current = new Token(Token.FLOATLITERAL, currentLexeme.toString(), posit_orig);
        madeToken = true;
        SourcePos posit;
        posit = new SourcePos();
        posit.StartLine = currentLineNr;
        posit.EndLine = currentLineNr;
        posit.StartCol = currentColNr+1;
        posit.EndCol = currentColNr+1;
        prevToken1 = new Token(Token.ID, Character.toString(temp), posit);
        //prevToken1.print();
        SourcePos posit2;
        posit2 = new SourcePos();
        posit2.StartLine = currentLineNr;
        posit2.EndLine = currentLineNr;
        posit2.StartCol = currentColNr+2;
        posit2.EndCol = currentColNr+2;
        if(temp2 == '+'){
          prevToken2 = new Token(Token.PLUS, Character.toString(temp2), posit2);
        }
        else{
          prevToken2 = new Token(Token.MINUS, Character.toString(temp2), posit2);
        }
        
      }
      else{
        currentColNr--;
        currentLexeme.delete(currentLexeme.length()-1,currentLexeme.length());
        SourcePos posit_orig;
        posit_orig = new SourcePos();
        posit_orig.StartLine = currentLineNr;
        posit_orig.EndLine = currentLineNr;
        posit_orig.StartCol = startColumn;
        posit_orig.EndCol = currentColNr;
        current = new Token(Token.FLOATLITERAL, currentLexeme.toString(), posit_orig);
        madeToken = true;
        SourcePos posit;
        posit = new SourcePos();
        posit.StartLine = currentLineNr;
        posit.EndLine = currentLineNr;
        posit.StartCol = currentColNr+1;
        posit.EndCol = currentColNr+1;
        prevToken1 = new Token(Token.ID, Character.toString(temp), posit);
      }
    }

  }

  private int scanToken() {
    
    switch (currentChar) {
    case '0':  case '1':  case '2':  case '3':  case '4':
    case '5':  case '6':  case '7':  case '8':  case '9':
      takeIt();
      while (isDigit(currentChar)) {
        currentColNr +=1;
        takeIt();
      } 
      if(currentChar == '.'){ // Digit+ .
        currentColNr +=1;
        takeIt();
        if(currentChar == 'E' || currentChar == 'e'){ // Digit+ . exponent
          exponent(currentChar);
        }
        else if(isDigit(currentChar)){ 
          currentColNr +=1;
          takeIt();
          while (isDigit(currentChar)) { // Digit+ . fraction
            currentColNr +=1;
            takeIt();
          }
          if(currentChar == 'E' || currentChar == 'e'){ // Digit+ . fraction exponent
            exponent(currentChar);
          }
        }
        
      }
      else if(currentChar == 'E' || currentChar == 'e'){ // Digit+ exponent
        exponent(currentChar);
      }
      else{
        return Token.INTLITERAL;
      }
      // Note: code for floating point literals is missing here...
      return Token.FLOATLITERAL;
    
    case '.':
        takeIt();
        if(isDigit(currentChar)){
          currentColNr +=1;
          takeIt();
        }
        else{
          return Token.ERROR;
        }
        while (isDigit(currentChar)) { // . fraction
          currentColNr +=1;
          takeIt();
        }
        if(currentChar == 'E' || currentChar == 'e'){ // . fraction exponent
          exponent(currentChar);
        }
        return Token.FLOATLITERAL;

    case '+':
        takeIt();
        return Token.PLUS;
    case '\u0000': // sourceFile.eot:
      currentLexeme.append('$');
      return Token.EOF;

    // Add code here for the remaining MiniC tokens...
    
    //Minus Checking
    case '-':
        takeIt();
        return Token.MINUS;

    //Times Checking
    case '*':
        takeIt();
        return Token.TIMES;

    //Div and Comments Checking
    case '/':
        takeIt();
        if (currentChar == '/'){
          takeIt();
          while(currentChar != '\n' && currentChar != '\u0000'){
            takeIt();
          }
          
          if(currentChar == '\n'){
            takeIt();
            currentLineNr++;
            currentColNr = 1;
            Comments = false;
          }
          currentLexeme = new StringBuffer("");
          return scanToken();
        }

        else if(currentChar == '*'){
          takeIt();
          currentColNr++;
          boolean endComments = false;
          while(!endComments){
            if(currentChar == '\n'){
              currentLineNr++;
              currentColNr = 1;
              takeIt();
            }
            else if(currentChar == '*'){
              takeIt();
              currentColNr++;
              if(currentChar == '/'){
                takeIt();
                currentColNr++;
                endComments = true;
                if(currentChar == '\n'){
                  takeIt();
                  currentLineNr++;
                  currentColNr = 1;
                }
                else if(currentChar == ' '){
                  takeIt();
                  currentColNr +=1;
                }
                else{
                  currentColNr +=1;
                }
                startLine = currentLineNr;
                startColumn = currentColNr;
                currentLexeme = new StringBuffer("");
                return scanToken();
              }
            }
            else if(currentChar == '\u0000'){
              endComments = true;
                Comments = false;
              System.out.println("ERROR: unterminated multi-line comment.");
              startLine = currentLineNr;
              startColumn = currentColNr;
              currentLexeme = new StringBuffer("");
              return scanToken();
            }
            else if(currentChar == '\n'){
              currentLineNr++;
              currentColNr = 1;
              takeIt();
              startLine = currentLineNr;
              startColumn = currentColNr;
            }
            else{
              currentColNr++;
              takeIt();
            }
          }

        }
        else{
          return Token.DIV;
        }
    //Semicolon Checking
    case ';':
        takeIt();
        return Token.SEMICOLON;

    //LeftBrace Checking
    case '{':
        takeIt();
        return Token.LEFTBRACE;

    //RightBrace Checking
    case '}':
        takeIt();
        return Token.RIGHTBRACE;

    //LeftBracket Checking
    case '[':
        takeIt();
        return Token.LEFTBRACKET;

    //RightBracket Checking
    case ']':
        takeIt();
        return Token.RIGHTBRACKET;

    //LeftParen Checking
    case '(':
        takeIt();
        return Token.LEFTPAREN;

    //RightParen Checking
    case ')':
        takeIt();
        return Token.RIGHTPAREN;

    //EQ, ASSIGN Checking
    case '=':
        takeIt();
        if (currentChar == '='){
          currentColNr +=1;
          takeIt();
          return Token.EQ;
        }
        else{
          return Token.ASSIGN;
        }
    
    //NOT, NOTEQ Checking
    case '!':
        takeIt();
        if (currentChar == '='){
          currentColNr +=1;
          takeIt();
          return Token.NOTEQ;
        }
        else{
          return Token.NOT;
        }

    //GREATER, GREATEREQ Checking
    case '>':
        takeIt();
        if (currentChar == '='){
          currentColNr +=1;
          takeIt();
          return Token.GREATEREQ;
        }
        else{
          return Token.GREATER;
        }
    //LESS, LESSEQ Checking
    case '<':
        takeIt();
        if (currentChar == '='){
          currentColNr +=1;
          takeIt();
          return Token.LESSEQ;
        }
        else{
          return Token.LESS;
        }
    //OR checking
    case '|':
      takeIt();
      if(currentChar == '|'){
        currentColNr +=1;
        takeIt();
        return Token.OR;
      }
      else{
        return Token.ERROR;
      }

    //AND checking
    case '&':
      takeIt();
      if(currentChar == '&'){
        currentColNr +=1;
        takeIt();
        return Token.AND;
      }
      else{
        return Token.ERROR;
      }

    //Comma Checking
    case ',':
      takeIt();
      return Token.COMMA;

    //String Checking

    case '\"':
      currentChar = sourceFile.readChar();
      while(currentChar != '\"'){
        
        if(currentChar == '\n' || currentChar == '\u0000'){
          System.out.println("ERROR: unterminated string literal");
          return Token.STRINGLITERAL;
        }
        else if(currentChar == '\\'){
          currentColNr++;
          takeIt();
          if(currentChar !='n'){
            System.out.println("ERROR: illegal escape sequence");
            currentColNr++;
            takeIt();
          }
          else{
            currentColNr++;
            takeIt();
          }
        }
        else{
          currentColNr++;
          takeIt();
        }
      }
      if(currentChar == '\"'){
        currentColNr++;
        currentChar = sourceFile.readChar();
        return Token.STRINGLITERAL;
      }

    //ID checking
    case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g': case 'h': case 'i': case 'j': case 'k':
    case 'l': case 'm': case 'n': case 'o': case 'p': case 'q': case 'r': case 's': case 't': case 'u': case 'v':
    case 'w': case 'x': case 'y': case 'z': case '_': case 'A': case 'B': case 'C': case 'D': case 'E': case 'F':
    case 'G': case 'H': case 'I': case 'J': case 'K': case 'L': case 'M': case 'N': case 'O': case 'P': case 'Q':
    case 'R': case 'S': case 'T': case 'U': case 'V': case 'W': case 'X': case 'Y': case 'Z':
      takeIt();
      while (isChar(currentChar) || isDigit(currentChar)){
        currentColNr +=1;
        takeIt();
        if(currentLexeme.toString().equals("true") || currentLexeme.toString().equals("false")){ // BOOLLITERAL
          
          if(!(isChar(currentChar) || isDigit(currentChar))){
            return Token.BOOLLITERAL;
          }
        }
        
      }  
      if(!(isChar(currentChar) || isDigit(currentChar))){
        return Token.ID;
      }

    default:
      takeIt();  
      return Token.ERROR;
    }
    
  }

  public Token scan() {
    Token currentToken;
    SourcePos pos;
    int kind;
    
    if(prev1Flag == true){
      prevToken1 = null;
      prev1Flag = false;
    }
    if(prev2Flag == true){
      prevToken2 = null;
      prev2Flag = false;
    }
    if(prevToken1 != null){
      prev1Flag = true;
      currentColNr++;
      if(verbose)
        prevToken1.print();
      return prevToken1;
    }
    else if(prevToken2 != null){
      prev2Flag = true;
      currentColNr++;
      if(verbose)
        prevToken2.print();
      return prevToken2;
    }

    currentlyScanningToken = false;
    while (currentChar == ' '
           || currentChar == '\f'
           || currentChar == '\n'
           || currentChar == '\r'
           || currentChar == '\t')
    {
      if(currentChar == '\n'){
        currentColNr = 1;
        currentLineNr += 1;        
      }
      else if(currentChar == ' '){
        currentColNr +=1;
      }
      
      takeIt();
    } 
    
    currentlyScanningToken = true;
    currentLexeme = new StringBuffer("");
    pos = new SourcePos();
    // Note: currentLineNr and currentColNr are not maintained yet!
    pos.StartLine = currentLineNr;
    pos.EndLine = currentLineNr;
    pos.StartCol = currentColNr;
    Comments = true;
    
    startColumn = currentColNr;
    startLine = currentLineNr;

    kind = scanToken();
    if(madeToken){
      currentToken = current;
      madeToken = false;
    }
    else{
      currentToken = new Token(kind, currentLexeme.toString(), pos);
    }

    if(startColumn != pos.StartCol){ // For /* */ Comments
      pos.StartCol = startColumn;
    }
    if(startLine != pos.StartLine){
      pos.StartLine = startLine;
      pos.EndLine = startLine;
    }

    if(Comments == false){ // For '//' Comments
      pos.StartLine = currentLineNr;
      pos.EndLine = currentLineNr;
      pos.StartCol = 1;
    }


    

    pos.EndCol = currentColNr;
    currentColNr +=1;
    //System.out.println(charCount);
    if (verbose)
      currentToken.print();
    return currentToken;
  }

}
