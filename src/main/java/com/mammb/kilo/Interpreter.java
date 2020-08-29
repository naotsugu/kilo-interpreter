package com.mammb.kilo;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Scanner;
import java.util.StringJoiner;
import java.util.stream.Collectors;
import static com.mammb.kilo.Interpreter.TokenType.*;

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
                rep(input);
            } catch (IOException e) {
                System.out.println(e.getMessage());
            }
            return;
        }

        var scanner = new Scanner(System.in);
        for(;;) {
            System.out.println("Type q to exit.");
            System.out.print("> ");
            String line = scanner.nextLine();
            if (line.equals("q")) break;
            rep(line);
        }
        scanner.close();
    }

    /**
     * Read Evaluate Print.
     * @param input Source
     */
    private static void rep(String input) {

        var parser = Parser.of(Lexer.of(input));
        Statements statements = parser.parse();
        for (var error : parser.errors) {
            System.out.println(error);
        }

        var evaluated = Evaluator.of().eval(statements);
        if (Objects.nonNull(evaluated)) {
            System.out.println(evaluated.inspect());
        }

    }

    /**
     * TokenType.
     */
    enum TokenType {

        ILLEGAL(""),
        EOF(""),
        IDENT(""),
        INT(""),

        // operator
        ASSIGN("="),
        PLUS("+"),
        MINUS("-"),
        BANG("!"),
        ASTER("*"),
        SLASH("/"),
        LT("<"),
        GT(">"),
        EQ("=="),
        NOT_EQ("!="),

        // delimiter
        COMMA(","),
        SEMIC(";"),

        LPAREN("("),
        RPAREN(")"),
        LBRACE("{"),
        RBRACE("}"),

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
                case '+' -> new Token(PLUS);
                case '-' -> new Token(MINUS);
                case '*' -> new Token(ASTER);
                case '/' -> new Token(SLASH);
                case '<' -> new Token(LT);
                case '>' -> new Token(GT);
                case '(' -> new Token(LPAREN);
                case ')' -> new Token(RPAREN);
                case '{' -> new Token(LBRACE);
                case '}' -> new Token(RBRACE);
                case ',' -> new Token(COMMA);
                case ';' -> new Token(SEMIC);
                case 0   -> new Token(EOF);
                case '=' -> nextIf('=') ? new Token(EQ) : new Token(ASSIGN);
                case '!' -> nextIf('=') ? new Token(NOT_EQ) : new Token(BANG);
                default -> {
                    if (isLetter(ch)) {
                        final String id = readIdentifier();
                        yield new Token(lookupIdent(id), id);
                    } else if (isDigit(ch)) {
                        yield new Token(INT, readNumber());
                    } else {
                        yield new Token(ILLEGAL, Character.toString(ch));
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
            Precedence precedence = Precedence.of(token.type);
            nextToken();
            Expression right = parseExpression(precedence);
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
            BlockStatement body = parseBlockStatement();
            return new FunctionLiteral(token, parameters, body);
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
            if (!expectPeek(TokenType.RPAREN)) {
                return null;
            }
            return identifiers;
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
            return new CallExpression(token, function, parseCallArguments());
        }

        private List<Expression> parseCallArguments() {
            var args = new ArrayList<Expression>();
            if (peekTokenIs(TokenType.RPAREN)) {
                nextToken();
                return Collections.emptyList();
            }
            nextToken();
            args.add(parseExpression(Precedence.LOWEST));
            while (peekTokenIs(TokenType.COMMA)) {
                nextToken();
                nextToken();
                args.add(parseExpression(Precedence.LOWEST));
            }
            if (!expectPeek(TokenType.RPAREN)) {
                return null;
            }
            return args;
        }

        private boolean curTokenIs(TokenType t) {
            return curToken.type == t;
        }

        private boolean peekTokenIs(TokenType t) {
            return peekToken.type == t;
        }

        private void peekError(TokenType t) {
            String msg = String.format("expected next token to be %s, got %s instead", t, peekToken.type);
            errors.add(msg);
        }

        /**
         * Parser of prefix operator.
         * like {@code ++5}
         */
        interface PrefixParser {

            Expression parse(Parser parser);

            /** Prefix parsing functions */
            Map<TokenType, PrefixParser> map = Map.of(
                    TokenType.IDENT,  Parser::parseIdentifier,
                    TokenType.INT,    Parser::parseIntegerLiteral,
                    TokenType.BANG,   Parser::parsePrefixExpression,
                    TokenType.MINUS,  Parser::parsePrefixExpression,
                    TokenType.TRUE,   Parser::parseBoolean,
                    TokenType.FALSE,  Parser::parseBoolean,
                    TokenType.LPAREN, Parser::parseGroupedExpression,
                    TokenType.IF,     Parser::parseIfExpression,
                    TokenType.FN,     Parser::parseFunctionLiteral
            );
        }

        /**
         * Parser of infix operator.
         * like {@code 5 * 8}
         */
        interface InfixParser {

            Expression parse(Parser parser, Expression expression);

            /** Infix parsing functions */
            Map<TokenType, InfixParser> map = Map.of(
                    TokenType.PLUS,   Parser::parseInfixExpression,
                    TokenType.MINUS,  Parser::parseInfixExpression,
                    TokenType.SLASH,  Parser::parseInfixExpression,
                    TokenType.ASTER,  Parser::parseInfixExpression,
                    TokenType.EQ,     Parser::parseInfixExpression,
                    TokenType.NOT_EQ, Parser::parseInfixExpression,
                    TokenType.LT,     Parser::parseInfixExpression,
                    TokenType.GT,     Parser::parseInfixExpression,
                    TokenType.LPAREN, Parser::parseCallExpression
            );
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
            ;

            private final List<TokenType> tokenTypes;

            Precedence(TokenType... tokenTypes) {
                this.tokenTypes = Arrays.asList(tokenTypes);
            }

            public static Precedence of(TokenType type) {
                return Arrays.stream(Precedence.values())
                        .filter(p -> p.tokenTypes.contains(type))
                        .findFirst()
                        .orElse(LOWEST);
            }

            public boolean isLessThan(Precedence that) {
                return this.compareTo(that) < 0;
            }
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

    static record ReturnStatement(
            Token token,
            Expression returnValue) implements Statement {
        @Override public String toString() {
            return token.literal + " " +
                    (Objects.isNull(returnValue) ? "" : returnValue.toString()) + ";";
        }
    }

    static record Identifier(
            Token token,
            String value) implements Expression {
        @Override public String toString() {
            return value;
        }
    }

    static record IntegerLiteral(
            Token token,
            Integer value) implements Expression {
        @Override public String toString() {
            return token.literal;
        }
    }

    static record BooleanLiteral(
            Token token,
            Boolean value) implements Expression {
        @Override public String toString() {
            return token.literal;
        }
    }

    static record ExpressionStatement(
            Token token,
            Expression expression) implements Statement {
        @Override public String toString() {
            return Objects.isNull(expression) ? "" : expression.toString();
        }
    }

    static record PrefixExpression(
            Token token,
            String operator,
            Expression right) implements Expression {
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

    static record BlockStatement(
            Token token,
            List<Statement> statements) implements Statement {
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
            sb.append("if");
            sb.append(condition.toString());
            sb.append(" ");
            sb.append(consequence.toString());
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
            sb.append("(");
            StringJoiner sj = new StringJoiner(", ");
            parameters.forEach(p -> sj.add(p.toString()));
            sb.append(sj.toString());
            sb.append(")");
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
            sb.append("(");

            StringJoiner sj = new StringJoiner(", ");
            arguments.forEach(a -> sj.add(a.toString()));
            sb.append(sj.toString());

            sb.append(")");
            return sb.toString();
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
        public Any eval(Node node) {
            return eval(node, env);
        }

        private Any eval(Node node, Environment env) {

            if (node instanceof Statements n) {
                return evalStatements(n, env);

            } else if (node instanceof ExpressionStatement n) {
                return eval(n.expression(), env);

            } else if (node instanceof IntegerLiteral n) {
                return new Int(n.value());

            } else if (node instanceof BooleanLiteral n) {
                return n.value() ? TRUE : FALSE;

            } else if (node instanceof PrefixExpression n) {
                var right = eval(n.right(), env);
                if (isError(right)) return right;
                return evalPrefixExpression(n.operator(), right);

            } else if (node instanceof InfixExpression n) {
                var left = eval(n.left(), env);
                if (isError(left)) return left;
                var right = eval(n.right(), env);
                if (isError(right)) return right;
                return evalInfixExpression(n.operator(), left, right);

            } else if (node instanceof BlockStatement n) {
                return evalBlockStatement(n, env);

            } else if (node instanceof IfExpression n) {
                return evalIfExpression(n, env);

            } else if (node instanceof ReturnStatement n) {
                var val = eval(n.returnValue(), env);
                if (isError(val)) return val;
                return new ReturnValue(val);

            } else if (node instanceof LetStatement n) {
                var val = eval(n.value(), env);
                if (isError(val)) return val;
                env.set(n.name().value(), val);

            } else if (node instanceof Identifier n) {
                return evalIdentifier(n, env);

            } else if (node instanceof FunctionLiteral n) {
                var params = n.parameters();
                var body = n.body();
                return new Function(params, body, env);

            } else if (node instanceof CallExpression n) {
                var func = eval(n.function(), env);
                if (isError(func)) return func;
                var args = evalExpressions(n.arguments(), env);
                if (args.size() == 1 && isError(args.get(0))) {
                    return args.get(0);
                }
                return applyFunction(func, args);
            }
            return null;
        }


        private Any evalStatements(Statements statements, Environment env) {
            Any result = null;
            for (Statement statement : statements.statements()) {
                result = eval(statement, env);
                if (result instanceof ReturnValue rv) {
                    return rv.value();
                } else if (result instanceof Error) {
                    return result;
                }
            }
            return result;
        }


        private Any evalBlockStatement(BlockStatement block, Environment env) {
            Any result = null;
            for (Statement statement : block.statements()) {
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
                case "+" -> new Int(leftVal + rightVal);
                case "-" -> new Int(leftVal - rightVal);
                case "*" -> new Int(leftVal * rightVal);
                case "/" -> new Int(leftVal / rightVal);
                case "<" -> (leftVal < rightVal) ? TRUE : FALSE;
                case ">" -> (leftVal > rightVal) ? TRUE : FALSE;
                case "==" -> (leftVal == rightVal) ? TRUE : FALSE;
                case "!=" -> (leftVal != rightVal) ? TRUE : FALSE;
                default -> Error.of("unknown operator: %s %s %s",
                        left.getClass().getSimpleName(), operator, right.getClass().getSimpleName());
            };
        }

        private List<Any> evalExpressions(List<Expression> exps, Environment env) {
            List<Any> result = new ArrayList<>();
            for (Expression exp : exps) {
                var evaluated = eval(exp, env);
                if (isError(evaluated)) {
                    return List.of(evaluated);
                }
                result.add(evaluated);
            }
            return result;
        }

        private Any evalIfExpression(IfExpression ie, Environment env) {
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

        private Any evalIdentifier(Identifier identifier, Environment env) {
            var val = env.get(identifier.value());
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

        private boolean isTruthy(Any any) {
            if (any == NULL) return false;
            if (any == TRUE) return true;
            if (any == FALSE) return false;
            return true;
        }

        private Any applyFunction(Any any, List<Any> args) {
            if (any instanceof Function fn) {
                var extendedEnv = extendFunctionEnv(fn, args);
                var evaluated = eval(fn.body(), extendedEnv);
                return unwrapReturnValue(evaluated);
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

        private boolean isError(Any any) {
            return any instanceof Error;
        }

        private static final Bool TRUE  = new Bool(Boolean.TRUE);
        private static final Bool FALSE = new Bool(Boolean.FALSE);
        private static final Null NULL  = new Null();

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

        private static record Int(Integer val) implements Any {
            @Override public String inspect() {
                return val.toString();
            }
        }

        private static record Bool(Boolean val) implements Any {
            @Override public String inspect() {
                return val.toString();
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

        private static record Function(
                List<Identifier> params,
                BlockStatement body,
                Environment env) implements Any {
            @Override public String inspect() {
                return String.format("fn(%s) {\n%s\n}",
                        params.stream().map(i -> i.toString()).collect(Collectors.joining()),
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
    }

}
