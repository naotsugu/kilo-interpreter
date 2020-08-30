package com.mammb.kilo;

import java.io.IOException;
import java.nio.file.*;
import java.util.*;
import java.util.stream.Collectors;
import static java.util.Map.entry;

/**
 * Interpreter.
 */
public class Interpreter {

    /**
     * Entry point.
     * @param args Command line args
     */
    public static void main(String[] args) {

        if (args.length > 0) {
            try {
                String input = Files.readString(Paths.get(args[0]));
                rep(Evaluator.of(), input);
            } catch (IOException e) {
                System.out.println(e.getMessage());
            }
            return;
        }

        System.out.println("Type q to exit.");
        var scanner = new Scanner(System.in);
        var evaluator = Evaluator.of();
        for(;;) {
            System.out.print("> ");
            String line = scanner.nextLine();
            if (line.equals("q")) break;
            rep(evaluator, line);
        }
        scanner.close();
    }

    /**
     * Read Evaluate Print.
     * @param input Source
     */
    private static void rep(Evaluator evaluator, String input) {

        var parser = Parser.of(Lexer.of(input));
        Parser.Statements statements = parser.parse();
        for (var error : parser.errors) {
            System.out.println(error);
        }

        var evaluated = evaluator.eval(statements);
        if (Objects.nonNull(evaluated)) {
            System.out.println(evaluated.inspect());
        }

    }

    /**
     * TokenType.
     */
    enum TokenType {

        ILLEGAL(""), EOF(""), IDENT(""), INT(""), STR(""),

        // operator
        ASSIGN("="),
        BANG("!"),
        PLUS("+"), MINUS("-"),
        ASTER("*"), SLASH("/"),
        LT("<"), GT(">"),
        EQ("=="), NOT_EQ("!="),

        // delimiter
        COMMA(","), SEMIC(";"), COLON(":"),

        LPAREN("("), RPAREN(")"),
        LBRACE("{"), RBRACE("}"),
        LBRACKET("["), RBRACKET("]"),

        // keywords
        FN("fn"),
        LET("let"),
        TRUE("true"),
        FALSE("false"),
        IF("if"),
        ELSE("else"),
        RETURN("return"),
        ;

        private final String str;

        private static Map<String, TokenType> keywords = Arrays
                .asList(FN, LET, TRUE, FALSE, IF, ELSE, RETURN).stream()
                .collect(Collectors.toMap(TokenType::str, t -> t));

        TokenType(String str) {
            this.str = str;
        }

        public String str() {
            return str;
        }

        public static TokenType lookupIdent(String ident) {
            return keywords.getOrDefault(ident, IDENT);
        }
    }

    /**
     * Token.
     */
    public record Token(TokenType type, String literal) {
        public Token(TokenType type) { this(type, type.str); }
    }


    /**
     * Lexer.
     * recursive descent parsing
     */
    static class Lexer {

        /** input string. */
        private final String input;

        /** current position in input (points to current char). */
        private int position;

        /** current reading position in input (after current char). */
        private int readPosition;

        /** current char under examination. */
        private int ch;

        private Lexer(String input) {
            this.input = input;
            this.readPosition = 0;
            readChar();
        }

        public static Lexer of(String input) {
            return new Lexer(input);
        }

        public Token nextToken() {
            skipWhitespace();
            Token token = switch (ch) {
                case '+' -> new Token(TokenType.PLUS);
                case '-' -> new Token(TokenType.MINUS);
                case '*' -> new Token(TokenType.ASTER);
                case '/' -> new Token(TokenType.SLASH);
                case '<' -> new Token(TokenType.LT);
                case '>' -> new Token(TokenType.GT);
                case '(' -> new Token(TokenType.LPAREN);
                case ')' -> new Token(TokenType.RPAREN);
                case '{' -> new Token(TokenType.LBRACE);
                case '}' -> new Token(TokenType.RBRACE);
                case ',' -> new Token(TokenType.COMMA);
                case ';' -> new Token(TokenType.SEMIC);
                case ':' -> new Token(TokenType.COLON);
                case '=' -> nextIf('=') ? new Token(TokenType.EQ) : new Token(TokenType.ASSIGN);
                case '!' -> nextIf('=') ? new Token(TokenType.NOT_EQ) : new Token(TokenType.BANG);
                case '"' -> new Token(TokenType.STR, readString());
                case '[' -> new Token(TokenType.LBRACKET);
                case ']' -> new Token(TokenType.RBRACKET);
                case 0   -> new Token(TokenType.EOF);
                default -> {
                    if (isLetter(ch)) {
                        final String id = readIdentifier();
                        yield new Token(TokenType.lookupIdent(id), id);
                    } else if (isDigit(ch)) {
                        yield new Token(TokenType.INT, readNumber());
                    } else {
                        yield new Token(TokenType.ILLEGAL, Character.toString(ch));
                    }
                }
            };
            readChar();
            return token;
        }

        private void readChar() {
            ch = (readPosition >= input.length()) ? 0 : input.codePointAt(readPosition);
            position = readPosition;
            readPosition += 1;
        }

        private String readIdentifier() {
            int pos = position;
            while (isLetter(ch)) {
                readChar();
            }
            if (pos != position) readPosition -= 1; // step back
            return input.substring(pos, position);
        }

        private String readNumber() {
            int pos = position;
            while (isDigit(ch)) {
                readChar();
            }
            if (pos != position) readPosition -= 1; // step back
            return input.substring(pos, position);
        }

        private String readString() {
            int pos = position + 1;
            do {
                readChar();
            } while (ch != '"' && ch != 0);
            return input.substring(pos, position);
        }

        private int peekChar() {
            return (readPosition >= input.length()) ? 0 : input.codePointAt(readPosition);
        }

        private boolean nextIf(int c) {
            if (peekChar() == c) {
                readChar();
                return true;
            }
            return false;
        }

        private void skipWhitespace() {
            while (isWhitespace(ch)) {
                readChar();
            }
        }

        private static boolean isLetter(int c) {
            return 'a' <= c && c <= 'z' || 'A' <= c && c <= 'Z' || c == '_';
        }

        private static boolean isDigit(int c) {
            return '0' <= c && c <= '9';
        }

        private static boolean isWhitespace(int c) {
            return c == ' ' || c == '\t' || c == '\n' || c == '\r';
        }

    }

    /**
     * Parser.
     */
    public static class Parser {

        /** Lexer. */
        private final Lexer lexer;

        private final List<String> errors = new ArrayList<>();

        /** current token in lexer. */
        private Token curToken;

        /** next token in lexer. */
        private Token peekToken;

        private Parser(Lexer lexer) {
            this.lexer = lexer;
            this.curToken  = lexer.nextToken();
            this.peekToken = lexer.nextToken();
        }

        public static Parser of(Lexer lexer) {
            return new Parser(lexer);
        }

        private void nextToken() {
            curToken = peekToken;
            peekToken = lexer.nextToken();
        }

        /**
         * Parse statements.
         * @return Parse result
         */
        public Statements parse() {
            var statements = new ArrayList<Statement>();
            while (curToken.type != TokenType.EOF) {
                Statement stm = parseStatement();
                if (stm != null) {
                    statements.add(stm);
                }
                nextToken();
            }
            return new Statements(statements);
        }

        /**
         * Parse statement.
         * Statement is [let] or [return] or [expression statement]
         * @return Statement
         */
        private Statement parseStatement()  {
            return switch (curToken.type) {
                case LET    -> parseLetStatement();
                case RETURN -> parseReturnStatement();
                default     -> parseExpressionStatement();
            };
        }

        /**
         * Assert next token type.
         * @param t expected token type
         * @return if next token is expected type, then return {@code true}
         */
        private boolean expectPeek(TokenType t) {
            if (peekTokenIs(t)) {
                nextToken();
                return true;
            } else {
                peekError(t);
                return false;
            }
        }

        /**
         * Parse let statement.
         * <pre>
         * let <identifier> = <expression>;
         * </pre>
         * @return LetStatement
         */
        private LetStatement parseLetStatement()  {

            final Token token = curToken;
            if (!expectPeek(TokenType.IDENT)) {
                return null;
            }
            Identifier identifier = parseIdentifier();

            if (!expectPeek(TokenType.ASSIGN)) {
                return null;
            }

            nextToken();
            Expression expression = parseExpression(Precedence.LOWEST);
            while (!curTokenIs(TokenType.SEMIC)) {
                nextToken();
            }

            return new LetStatement(token, identifier, expression);
        }

        /**
         * Parse return statement.
         * <pre>
         * return <expression>;
         * </pre>
         * @return ReturnStatement
         */
        private ReturnStatement parseReturnStatement() {

            final Token token = curToken;
            nextToken();
            Expression expression = parseExpression(Precedence.LOWEST);

            while (!curTokenIs(TokenType.SEMIC)) {
                nextToken();
            }

            return new ReturnStatement(token, expression);
        }

        /**
         * Parse expression statement.
         * @return ExpressionStatement
         */
        private ExpressionStatement parseExpressionStatement() {

            final Token token = curToken;

            final Expression expression = parseExpression(Precedence.LOWEST);
            if (peekTokenIs(TokenType.SEMIC)) {
                nextToken();
            }

            return new ExpressionStatement(token, expression);
        }

        /**
         * Parse expression.
         * <pre>
         *  5 * 5 + 10 -> ((5 * 5) + 10)
         * </pre>
         * @param precedence
         * @return
         */
        private Expression parseExpression(Precedence precedence) {

            Expression leftExp = PrefixParser.map.containsKey(curToken.type)
                    ? PrefixParser.map.get(curToken.type).parse(this)
                    : null;

            while (!peekTokenIs(TokenType.SEMIC) &&
                    precedence.isLessThan(Precedence.of(peekToken.type))) {
                if (InfixParser.map.containsKey(peekToken.type)) {
                    InfixParser infix = InfixParser.map.get(peekToken.type);
                    nextToken();
                    leftExp = infix.parse(this, leftExp);
                } else {
                    return leftExp;
                }
            }

            return leftExp;
        }

        /**
         * Parse identifier.
         * @return Identifier
         */
        private Identifier parseIdentifier() {
            return new Identifier(curToken, curToken.literal);
        }

        /**
         * Parse boolean literal.
         * @return BooleanLiteral
         */
        private BooleanLiteral parseBoolean() {
            return new BooleanLiteral(curToken, curTokenIs(TokenType.TRUE));
        }

        /**
         * Parse integer literal.
         * @return IntegerLiteral
         */
        private IntegerLiteral parseIntegerLiteral() {
            try {
                Integer val = Integer.parseInt(curToken.literal);
                return new IntegerLiteral(curToken, val);
            } catch (NumberFormatException e) {
                errors.add(String.format("could not parse %q as integer", curToken.literal));
            }
            return null;
        }

        /**
         * Parse string literal.
         * @return StringLiteral
         */
        private StringLiteral parseStringLiteral() {
            return new StringLiteral(curToken, curToken.literal);
        }

        /**
         * Parse prefix expression.
         * like {@code !foo;}
         * <pre>
         *     <prefix operator><expression>;
         * </pre>
         * @return PrefixExpression
         */
        private PrefixExpression parsePrefixExpression() {
            Token token = curToken;
            nextToken();
            final Expression expression = parseExpression(Precedence.PREFIX);
            return new PrefixExpression(token, token.literal, expression);
        }

        /**
         * Parse infix expression.
         * like {@code 5 * 5;}
         * <pre>
         *     <expression> <infix operator> <expression>
         * </pre>
         * @param left Left expression
         * @return InfixExpression
         */
        private InfixExpression parseInfixExpression(Expression left) {
            Token token = curToken;
            String operator = curToken.literal;
            nextToken();
            Expression right = parseExpression(Precedence.of(token.type));
            return new InfixExpression(token, left, operator, right);
        }

        /**
         * Parse grouped expression.
         * @return Expression
         */
        private Expression parseGroupedExpression() {
            nextToken();
            Expression exp = parseExpression(Precedence.LOWEST);
            if (!expectPeek(TokenType.RPAREN)) {
                return null;
            }
            return exp;
        }

        /**
         * Parse if expression.
         * <pre>
         *     if (<condition>) <consequence> else <alternative>
         * </pre>
         * @return
         */
        private Expression parseIfExpression() {
            final Token token = curToken;
            if (!expectPeek(TokenType.LPAREN)) {
                return null;
            }
            nextToken();
            final Expression condition = parseExpression(Precedence.LOWEST);
            if (!expectPeek(TokenType.RPAREN)) {
                return null;
            }
            if (!expectPeek(TokenType.LBRACE)) {
                return null;
            }
            final BlockStatement consequence = parseBlockStatement();

            BlockStatement alternative = null;
            if (peekTokenIs(TokenType.ELSE)) {
                nextToken();
                if (!expectPeek(TokenType.LBRACE)) {
                    return null;
                }
                alternative = parseBlockStatement();
            }
            return new IfExpression(token, condition, consequence, alternative);
        }

        /**
         * Parse function literal.
         * like {@code fn(x, y) { return x + y; }}
         * <pre>
         *     fn <parameters> <block statement>
         * </pre>
         * @return FunctionLiteral
         */
        private FunctionLiteral parseFunctionLiteral() {
            final Token token = curToken;
            if (!expectPeek(TokenType.LPAREN)) {
                return null;
            }
            List<Identifier> parameters = parseFunctionParameters();
            if (!expectPeek(TokenType.LBRACE)) {
                return null;
            }
            return new FunctionLiteral(token, parameters, parseBlockStatement());
        }

        private BlockStatement parseBlockStatement() {
            final Token token = curToken;
            nextToken();
            List<Statement> statements = new ArrayList<>();
            while (!curTokenIs(TokenType.RBRACE) && !curTokenIs(TokenType.EOF)) {
                Statement stmt = parseStatement();
                if (stmt != null) {
                    statements.add(stmt);
                }
                nextToken();
            }
            return new BlockStatement(token, statements);
        }

        private List<Identifier> parseFunctionParameters() {
            var identifiers = new ArrayList<Identifier>();
            if (peekTokenIs(TokenType.RPAREN)) {
                nextToken();
                return identifiers;
            }
            nextToken();

            identifiers.add(new Identifier(curToken, curToken.literal));
            while (peekTokenIs(TokenType.COMMA)) {
                nextToken();
                nextToken();
                identifiers.add(new Identifier(curToken, curToken.literal));
            }
            return expectPeek(TokenType.RPAREN) ? identifiers : null;
        }

        /**
         * Parse call expression.
         * <pre>
         *     <expression>(<comma separated expressions>)
         * </pre>
         * @param function
         * @return CallExpression
         */
        private CallExpression parseCallExpression(Expression function) {
            Token token = curToken;
            List<Expression> args = parseExpressionList(TokenType.RPAREN);
            return new CallExpression(token, function, args);
        }

        /**
         * Parse index expression.
         * <pre>
         *     <expression>[<expression>]
         * </pre>
         * @param left
         * @return IndexExpression
         */
        private IndexExpression parseIndexExpression(Expression left) {
            Token token = curToken;
            nextToken();
            Expression index = parseExpression(Precedence.LOWEST);
            return expectPeek(TokenType.RBRACKET) ? new IndexExpression(token, left, index) : null;
        }

        /**
         * Parse array literal.
         * @return ArrayLiteral
         */
        private ArrayLiteral parseArrayLiteral() {
            return new ArrayLiteral(curToken, parseExpressionList(TokenType.RBRACKET));
        }

        /**
         * Parse hash literal.
         * <pre>
         *     {<expression> : <expression>, <expression> : <expression>, ... }
         * </pre>
         * @return HashLiteral
         */
        private HashLiteral parseHashLiteral() {
            Token token = curToken;
            var pairs = new HashMap<Expression, Expression>();
            while (!peekTokenIs(TokenType.RBRACE)) {
                nextToken();
                var key = parseExpression(Precedence.LOWEST);
                if (!expectPeek(TokenType.COLON)) {
                    return null;
                }
                nextToken();
                var value = parseExpression(Precedence.LOWEST);
                pairs.put(key, value);
                if (!peekTokenIs(TokenType.RBRACE) && !expectPeek(TokenType.COMMA)) {
                    return null;
                }
            }
            if (!expectPeek(TokenType.RBRACE)) {
                return null;
            }
            return new HashLiteral(token, pairs);
        }

        private List<Expression> parseExpressionList(TokenType end) {
            var list = new ArrayList<Expression>();
            if (peekTokenIs(end)) {
                nextToken();
                return list;
            }
            nextToken();
            list.add(parseExpression(Precedence.LOWEST));
            while (peekTokenIs(TokenType.COMMA)) {
                nextToken();
                nextToken();
                list.add(parseExpression(Precedence.LOWEST));
            }
            return expectPeek(end) ? list : null;
        }

        private boolean curTokenIs(TokenType t) {
            return curToken.type == t;
        }

        private boolean peekTokenIs(TokenType t) {
            return peekToken.type == t;
        }

        private void peekError(TokenType t) {
            errors.add(String.format("expected next token to be %s, got %s instead", t, peekToken.type));
        }

        /**
         * Parser of prefix operator.
         * like {@code ++5}
         */
        interface PrefixParser {

            Expression parse(Parser parser);

            /** Prefix parsing functions */
            Map<TokenType, PrefixParser> map = Map.ofEntries(
                    entry(TokenType.IDENT,    Parser::parseIdentifier),
                    entry(TokenType.INT,      Parser::parseIntegerLiteral),
                    entry(TokenType.STR,      Parser::parseStringLiteral),
                    entry(TokenType.BANG,     Parser::parsePrefixExpression),
                    entry(TokenType.MINUS,    Parser::parsePrefixExpression),
                    entry(TokenType.TRUE,     Parser::parseBoolean),
                    entry(TokenType.FALSE,    Parser::parseBoolean),
                    entry(TokenType.LPAREN,   Parser::parseGroupedExpression),
                    entry(TokenType.IF,       Parser::parseIfExpression),
                    entry(TokenType.FN,       Parser::parseFunctionLiteral),
                    entry(TokenType.LBRACKET, Parser::parseArrayLiteral),
                    entry(TokenType.LBRACE,   Parser::parseHashLiteral));
        }

        /**
         * Parser of infix operator.
         * like {@code 5 * 8}
         */
        interface InfixParser {

            Expression parse(Parser parser, Expression expression);

            /** Infix parsing functions */
            Map<TokenType, InfixParser> map = Map.of(
                    TokenType.PLUS,     Parser::parseInfixExpression,
                    TokenType.MINUS,    Parser::parseInfixExpression,
                    TokenType.SLASH,    Parser::parseInfixExpression,
                    TokenType.ASTER,    Parser::parseInfixExpression,
                    TokenType.EQ,       Parser::parseInfixExpression,
                    TokenType.NOT_EQ,   Parser::parseInfixExpression,
                    TokenType.LT,       Parser::parseInfixExpression,
                    TokenType.GT,       Parser::parseInfixExpression,
                    TokenType.LPAREN,   Parser::parseCallExpression,
                    TokenType.LBRACKET, Parser::parseIndexExpression);
        }

        /**
         * Precedence.
         */
        public enum Precedence {
            LOWEST,
            EQUALS(TokenType.EQ, TokenType.NOT_EQ),
            LG(TokenType.LT, TokenType.GT),
            SUM(TokenType.PLUS, TokenType.MINUS),
            PROD(TokenType.ASTER, TokenType.SLASH),
            PREFIX,
            CALL(TokenType.LPAREN),
            INDEX(TokenType.LBRACKET);

            private final List<TokenType> tokenTypes;

            Precedence(TokenType... tokenTypes) {
                this.tokenTypes = Arrays.asList(tokenTypes);
            }

            public static Precedence of(TokenType type) {
                return Arrays.stream(Precedence.values())
                        .filter(p -> p.tokenTypes.contains(type))
                        .findFirst().orElse(LOWEST);
            }

            public boolean isLessThan(Precedence that) {
                return this.compareTo(that) < 0;
            }
        }

        public interface Node { }
        public interface Statement extends Node { }
        public interface Expression extends Node { }

        static record Statements(List<Statement> statements) implements Node {
            @Override public String toString() {
                StringBuilder sb = new StringBuilder();
                for (Statement statement : statements) {
                    sb.append(statement.toString());
                }
                return sb.toString();
            }
        }

        static record LetStatement(
                Token token,
                Identifier name,
                Expression value) implements Statement {
            @Override public String toString() {
                return token.literal + " " +
                        name.toString() + " = " +
                        (Objects.isNull(value) ? "" : value.toString()) + ";";
            }
        }

        static record ReturnStatement(Token token, Expression returnValue) implements Statement {
            @Override public String toString() {
                return token.literal + " " +
                        (Objects.isNull(returnValue) ? "" : returnValue.toString()) + ";";
            }
        }

        static record Identifier(Token token, String value) implements Expression {
            @Override public String toString() {
                return value;
            }
        }

        static record IntegerLiteral(Token token, Integer value) implements Expression {
            @Override public String toString() {
                return token.literal;
            }
        }

        static record StringLiteral(Token token, String value) implements Expression {
            @Override public String toString() {
                return token.literal;
            }
        }

        static record BooleanLiteral(Token token, Boolean value) implements Expression {
            @Override public String toString() {
                return token.literal;
            }
        }

        static record ArrayLiteral(Token token, List<Expression> elements) implements Expression {
            @Override public String toString() {
                return elements.stream().map(Expression::toString).collect(Collectors.joining(", ", "[", "]"));
            }
        }

        static record HashLiteral(Token token, Map<Expression, Expression> pairs) implements Expression {
            @Override public String toString() {
                return pairs.entrySet().stream().map(e -> e.getKey().toString() + ":" + e.getValue().toString())
                        .collect(Collectors.joining(", ", "{", "}"));
            }
        }

        static record ExpressionStatement(Token token, Expression expression) implements Statement {
            @Override public String toString() {
                return Objects.isNull(expression) ? "" : expression.toString();
            }
        }

        static record PrefixExpression(Token token, String operator, Expression right) implements Expression {
            @Override public String toString() {
                return "(" + operator + right.toString() + ')';
            }
        }

        static record InfixExpression(
                Token token,
                Expression left,
                String operator,
                Expression right) implements Expression {
            @Override public String toString() {
                return "(" + left.toString() + " " + operator + " " + right.toString() + ")";
            }
        }

        static record BlockStatement(Token token, List<Statement> statements) implements Statement {
            @Override public String toString() {
                StringBuilder sb = new StringBuilder();
                statements.forEach(sb::append);
                return sb.toString();
            }
        }

        static record IfExpression(
                Token token,
                Expression condition,
                BlockStatement consequence,
                BlockStatement alternative) implements Expression {
            @Override
            public String toString() {
                StringBuilder sb = new StringBuilder();
                sb.append("if ").append(condition.toString());
                sb.append(" ").append(consequence.toString());
                if (Objects.nonNull(alternative)) sb.append("else " + alternative.toString());
                return sb.toString();
            }
        }

        static record FunctionLiteral(
                Token token,
                List<Identifier> parameters,
                BlockStatement body) implements Expression {
            @Override public String toString() {
                StringBuilder sb = new StringBuilder();
                sb.append(token.literal);
                sb.append(parameters.stream().map(Identifier::toString).collect(Collectors.joining(", ", "(", ")")));
                sb.append(body.toString());
                return sb.toString();
            }
        }

        static record CallExpression(
                Token token,
                Expression function,
                List<Expression> arguments) implements Expression {
            @Override public String toString() {
                StringBuilder sb = new StringBuilder();
                sb.append(function.toString());
                sb.append(arguments.stream().map(Expression::toString).collect(Collectors.joining(", ", "(", ")")));
                return sb.toString();
            }
        }

        static record IndexExpression(
                Token token, // '['
                Expression left,
                Expression index) implements Expression {
            @Override public String toString() {
                StringBuilder sb = new StringBuilder();
                sb.append("(");
                sb.append(left().toString());
                sb.append("[");
                sb.append(index().toString());
                sb.append("])");
                return sb.toString();
            }
        }
    }


    /**
     * Evaluator of AST.
     */
    public static class Evaluator {

        /** Context for during evaluation. */
        private final Environment env;

        private Evaluator(Environment env) {
            this.env = env;
        }

        public static Evaluator of() {
            return new Evaluator(Environment.of());
        }

        /**
         * Evaluate.
         * @param node Node to evaluate
         * @return Evaluated object
         */
        public Any eval(Parser.Node node) {
            return eval(node, env);
        }

        private Any eval(Parser.Node node, Environment env) {

            if (node instanceof Parser.Statements n) {
                return evalStatements(n, env);

            } else if (node instanceof Parser.ExpressionStatement n) {
                return eval(n.expression(), env);

            } else if (node instanceof Parser.IntegerLiteral n) {
                return new Int(n.value());

            } else if (node instanceof Parser.StringLiteral n) {
                return new Str(n.value());

            } else if (node instanceof Parser.BooleanLiteral n) {
                return n.value() ? TRUE : FALSE;

            } else if (node instanceof Parser.PrefixExpression n) {
                var right = eval(n.right(), env);
                if (isError(right)) return right;
                return evalPrefixExpression(n.operator(), right);

            } else if (node instanceof Parser.InfixExpression n) {
                var left = eval(n.left(), env);
                if (isError(left)) return left;
                var right = eval(n.right(), env);
                if (isError(right)) return right;
                return evalInfixExpression(n.operator(), left, right);

            } else if (node instanceof Parser.BlockStatement n) {
                return evalBlockStatement(n, env);

            } else if (node instanceof Parser.IfExpression n) {
                return evalIfExpression(n, env);

            } else if (node instanceof Parser.ReturnStatement n) {
                var val = eval(n.returnValue(), env);
                if (isError(val)) return val;
                return new ReturnValue(val);

            } else if (node instanceof Parser.LetStatement n) {
                var val = eval(n.value(), env);
                if (isError(val)) return val;
                env.set(n.name().value(), val);

            } else if (node instanceof Parser.Identifier n) {
                return evalIdentifier(n, env);

            } else if (node instanceof Parser.FunctionLiteral n) {
                return new Function(n.parameters(), n.body(), env);

            } else if (node instanceof Parser.CallExpression n) {
                var func = eval(n.function(), env);
                if (isError(func)) return func;
                var args = evalExpressions(n.arguments(), env);
                if (args.size() == 1 && isError(args.get(0))) {
                    return args.get(0);
                }
                return applyFunction(func, args);

            } else if (node instanceof Parser.ArrayLiteral n) {
                var elements = evalExpressions(n.elements(), env);
                if (elements.size() == 1 && isError(elements.get(0))) {
                    return elements.get(0);
                }
                return new Array(elements);

            } else if (node instanceof Parser.HashLiteral n) {
                return evalHashLiteral(n, env);

            } else if (node instanceof Parser.IndexExpression ie) {
                var left = eval(ie.left(), env);
                if (isError(left)) return left;
                var index = eval(ie.index(), env);
                if (isError(index)) return index;
                return evalIndexExpression(left, index);
            }
            return null;
        }


        private Any evalStatements(Parser.Statements statements, Environment env) {
            Any result = null;
            for (Parser.Statement statement : statements.statements()) {
                result = eval(statement, env);
                if (result instanceof ReturnValue rv) {
                    return rv.value();
                } else if (result instanceof Error) {
                    return result;
                }
            }
            return result;
        }


        private Any evalBlockStatement(Parser.BlockStatement block, Environment env) {
            Any result = null;
            for (Parser.Statement statement : block.statements()) {
                result = eval(statement, env);
                if (result != null) {
                    if (result instanceof ReturnValue || result instanceof Error) {
                        return result;
                    }
                }
            }
            return result;
        }

        private Any evalPrefixExpression(String operator, Any right) {
            return switch (operator) {
                case "!" -> evalBangOperatorExpression(right);
                case "-" -> evalMinusPrefixOperatorExpression(right);
                default  -> Error.of("unknown operator: %s%s", operator, right.getClass().getSimpleName());
            };
        }

        private Any evalInfixExpression(String operator, Any left, Any right) {
            if (left instanceof Int l && right instanceof Int r) {
                return evalIntegerInfixExpression(operator, l, r);

            } else if (left instanceof Str l && right instanceof Str r) {
                    return evalStringInfixExpression(operator, l, r);

            } else if (operator.equals("==")) {
                return (left == right) ? TRUE : FALSE;

            } else if (operator.equals("!=")) {
                return (left != right) ? TRUE : FALSE;

            } else if (left.getClass() != right.getClass()) {
                return Error.of("type mismatch: %s %s %s",
                        left.getClass().getSimpleName(), operator, right.getClass().getSimpleName());

            } else {
                return Error.of("unknown operator: %s %s %s",
                        left.getClass().getSimpleName(), operator, right.getClass().getSimpleName());
            }
        }

        private Any evalIntegerInfixExpression(String operator, Int left, Int right) {
            int leftVal = left.val();
            int rightVal = right.val();
            return switch (operator) {
                case "+"  -> new Int(leftVal + rightVal);
                case "-"  -> new Int(leftVal - rightVal);
                case "*"  -> new Int(leftVal * rightVal);
                case "/"  -> new Int(leftVal / rightVal);
                case "<"  -> (leftVal < rightVal) ? TRUE : FALSE;
                case ">"  -> (leftVal > rightVal) ? TRUE : FALSE;
                case "==" -> (leftVal == rightVal) ? TRUE : FALSE;
                case "!=" -> (leftVal != rightVal) ? TRUE : FALSE;
                default   -> Error.of("unknown operator: %s %s %s",
                        left.getClass().getSimpleName(), operator, right.getClass().getSimpleName());
            };
        }

        private Any evalStringInfixExpression(String operator, Str left, Str right) {
            return "+".equals(operator)
                ? new Str(left.val() + right.val())
                : Error.of("unknown operator: %s %s %s",
                    left.getClass().getSimpleName(), operator, right.getClass().getSimpleName());
        }

        private List<Any> evalExpressions(List<Parser.Expression> exps, Environment env) {
            List<Any> result = new ArrayList<>();
            for (Parser.Expression exp : exps) {
                var evaluated = eval(exp, env);
                if (isError(evaluated)) {
                    return List.of(evaluated);
                }
                result.add(evaluated);
            }
            return result;
        }

        private Any evalIfExpression(Parser.IfExpression ie, Environment env) {
            var condition = eval(ie.condition(), env);
            if (isError(condition)) {
                return condition;
            } else if (isTruthy(condition)) {
                return eval(ie.consequence(), env);
            } else if (ie.alternative() != null) {
                return eval(ie.alternative(), env);
            } else {
                return NULL;
            }
        }

        private Any evalIndexExpression(Any left, Any index) {
            if (left instanceof Array array && index instanceof Int idx) {
                return evalArrayIndexExpression(array, idx);
            } else if (left instanceof Hash hash) {
                return evalHashIndexExpression(hash, index);
            } else {
                return Error.of("index operator not supported: %s", left.getClass().getSimpleName());
            }
        }

        private Any evalArrayIndexExpression(Array array, Int index) {
            return (index.val() < 0 || index.val() > (array.elements().size() - 1))
                    ? NULL
                    : array.elements().get(index.val());
        }

        private Any evalHashIndexExpression(Hash hash, Any key) {
            if (key instanceof Hashable hashable) {
                HashPair pair = hash.hash().get(hashable.hashKey());
                return Objects.nonNull(pair) ? pair.value() : null;
            } else {
                return Error.of("unusable as hash key: %s", key.getClass().getSimpleName());
            }
        }

        private Any evalIdentifier(Parser.Identifier identifier, Environment env) {
            var val = env.get(identifier.value());
            if (Objects.isNull(val)) {
                val = builtins.get(identifier.value());
            }
            if (Objects.isNull(val)) {
                return Error.of("identifier not found: " + identifier.value());
            }
            return val;
        }

        private Any evalBangOperatorExpression(Any right) {
            if (right == TRUE) return FALSE;
            if (right == FALSE) return TRUE;
            if (right == NULL) return TRUE;
            return FALSE;
        }

        private Any evalMinusPrefixOperatorExpression(Any right) {
            return (right instanceof Int i)
                    ? new Int(-i.val())
                    : Error.of("unknown operator: -%s", right.getClass().getSimpleName());
        }

        private Any evalHashLiteral(Parser.HashLiteral node, Environment env) {
            var pairs = new HashMap<HashKey, HashPair>();
            for (var entry : node.pairs.entrySet()) {
                var key = eval(entry.getKey(), env);
                if (isError(key)) return key;
                if (key instanceof Hashable hashable) {
                    var value = eval(entry.getValue(), env);
                    if (isError(value)) return value;
                    pairs.put(hashable.hashKey(), new HashPair(key, value));
                } else {
                    return Error.of("unusable as hash key: %s", key.getClass().getSimpleName());
                }
            }
            return new Hash(pairs);
        }

        private Any applyFunction(Any any, List<Any> args) {
            if (any instanceof Function fn) {
                var extendedEnv = extendFunctionEnv(fn, args);
                var evaluated = eval(fn.body(), extendedEnv);
                return unwrapReturnValue(evaluated);
            } else if (any instanceof Builtin bi) {
                return bi.fn.apply(args);
            } else {
                return Error.of("not a function: %s", any.getClass().getSimpleName());
            }
        }

        private Environment extendFunctionEnv(Function fn, List<Any> args) {
            var env = fn.env().createEnclosed();
            for (int i = 0; i < fn.params().size(); i++) {
                env.set(fn.params().get(i).value(), args.get(i));
            }
            return env;
        }

        private Any unwrapReturnValue(Any any) {
            return (any instanceof ReturnValue rv) ? rv.value() : any;
        }

        private boolean isTruthy(Any any) {
            return any != NULL && any != FALSE;
        }

        private boolean isError(Any any) {
            return any instanceof Error;
        }

        private static final Bool TRUE  = new Bool(Boolean.TRUE);
        private static final Bool FALSE = new Bool(Boolean.FALSE);
        private static final Null NULL  = new Null();

        /**
         * Environment.
         */
        private static record Environment(Map<String, Any> map, Environment outer) {

            public static Environment of() {
                return new Environment(new HashMap<>(), null);
            }

            public Environment createEnclosed() {
                return new Environment(new HashMap<>(), this);
            }

            public Any get(String name) {
                var any = map.get(name);
                if (Objects.isNull(any) && Objects.nonNull(outer)) {
                    any = outer.get(name);
                }
                return any;
            }

            public Any set(String name, Any any) {
                return map.put(name, any);
            }
        }


        interface Any {
            String inspect();
        }

        private static record Int(Integer val) implements Any, Hashable {
            @Override public String inspect() {
                return val.toString();
            }
            @Override public HashKey hashKey() {
                return new HashKey(Int.class, val);
            }
        }

        private static record Str(String val) implements Any, Hashable {
            @Override public String inspect() {
                return val;
            }
            @Override public HashKey hashKey() {
                return new HashKey(Str.class, val.hashCode());
            }
        }

        private static record Bool(Boolean val) implements Any, Hashable {
            @Override public String inspect() {
                return val.toString();
            }
            @Override public HashKey hashKey() {
                return new HashKey(Bool.class, val ? 1 : 0);
            }
        }

        private static record Null() implements Any {
            @Override public String inspect() {
                return "null";
            }
        }

        private static record ReturnValue(Any value) implements Any {
            @Override public String inspect() {
                return value.inspect();
            }
        }

        private static record Array(List<Any> elements) implements Any {
            @Override public String inspect() {
                return elements.stream().map(Any::inspect).collect(Collectors.joining(", ", "[", "]"));
            }
        }

        private interface Hashable {
            HashKey hashKey();
        }
        private static record HashPair(Any key, Any value) { }
        private static record HashKey(Class<? extends Hashable> type, int value) { }
        private static record Hash(Map<HashKey, HashPair> hash) implements Any {
            @Override public String inspect() {
                return hash.values().stream().map(e -> e.key().inspect() + ":" + e.value().inspect())
                        .collect(Collectors.joining(", ", "{", "}"));
            }
        }

        private static record Function(
                List<Parser.Identifier> params, Parser.BlockStatement body, Environment env) implements Any {
            @Override public String inspect() {
                return String.format("fn(%s) {\n%s\n}",
                        params.stream().map(Parser.Identifier::toString).collect(Collectors.joining()),
                        body.toString());
            }
        }

        private static record Error(String message) implements Any {
            public static Error of(String format, Object... obj) {
                return new Error(String.format(format, obj));
            }
            @Override public String inspect() {
                return "ERROR: " + message;
            }
        }

        private static record Builtin(java.util.function.Function<List<Any>, Any> fn) implements Any {
            @Override public String inspect() {
                return "builtin function";
            }
        }

        /** Builtin functions. */
        private static final Map<String, Builtin> builtins = Map.of(
                "len", new Builtin(args -> {
                    if (args.size() != 1) {
                        return Error.of("wrong number of arguments. got=%d, want=1", args.size());
                    } else if (args.get(0) instanceof Str str) {
                        return new Int(str.val().length());
                    } else if (args.get(0) instanceof Array array) {
                        return new Int(array.elements().size());
                    } else {
                        return Error.of("argument to `len` not supported, got %s",
                                args.get(0).getClass().getSimpleName());
                    }
                })
        );
    }

}
