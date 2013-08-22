// Generated by CoffeeScript 1.6.3
(function() {
  var IDENTIFIER_CHARS, arg, argc, e, expression, i, k, last, p, parse, parseError, parseForRacket, printTokens, printTree, racket, stringify, tokenizer, tokens, v, verbose, _exports, _ref,
    __slice = [].slice;

  IDENTIFIER_CHARS = '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ-+*/%^_$<>=?!';

  parseError = function(token, message) {
    throw new SyntaxError("" + message + " at " + token.file + ":" + token.line + ":" + token.character);
  };

  last = function(thing) {
    return thing[thing.length - 1];
  };

  tokenizer = (function() {
    var isDigit, isIdentifier, isSpace, symbol;
    symbol = {
      '(': 'group start',
      ')': 'group end',
      '{': 'funject start',
      '}': 'funject end',
      '[': 'list start',
      ',': 'list delimiter',
      ']': 'list end',
      ':': 'pattern match',
      '=': 'strict assignment',
      ':=': 'lazy assignment',
      '|=': 'reset strict assignment',
      '|:=': 'reset lazy assignment',
      '<-': 'inverse assignment',
      '<<': 'inheritance assignment',
      '::': 'prototypal application'
    };
    isSpace = function(c) {
      return c === ' ' || c === '\t';
    };
    isDigit = function(c) {
      return -1 !== '0123456789'.indexOf(c);
    };
    isIdentifier = function(c) {
      return -1 !== IDENTIFIER_CHARS.indexOf(c);
    };
    return function(raw, file, startLine) {
      var a, advance, b, c, character, here, i, indent, j, lastNewline, length, line, next, pairs, prev, s, start, syntaxError, token, tokens;
      if (file == null) {
        file = '<anonymous>';
      }
      if (startLine == null) {
        startLine = 1;
      }
      token = function(type, value) {
        if (value == null) {
          value = '';
        }
        return {
          type: type,
          value: value,
          line: line,
          file: file,
          character: character
        };
      };
      syntaxError = function(message) {
        throw new SyntaxError("" + message + " at " + file + ":" + line + ":" + character);
      };
      s = '';
      i = 0;
      while (true) {
        j = raw.indexOf('#', i);
        if (j === -1) {
          break;
        }
        s += raw.substring(i, j);
        i = j + 1;
        c = raw[i];
        if (c === '|') {
          ++i;
          start = i;
          pairs = 1;
          while (pairs) {
            a = raw.indexOf('#|', i);
            b = raw.indexOf('|#', i);
            if (a === -1 && b === -1) {
              throw new SyntaxError("Unmatched multiline comment");
            }
            if (a !== -1 && (a < b || b === -1)) {
              ++pairs;
              i = a + 2;
            }
            if (b !== -1 && (b < a || a === -1)) {
              --pairs;
              i = b + 2;
            }
          }
          s += Array(raw.substring(start, i).split('\n').length).join('\n');
        } else {
          j = raw.indexOf('\n', i);
          if (j === -1) {
            break;
          }
          i = j;
        }
      }
      s += raw.substring(i);
      i = 0;
      character = 0;
      lastNewline = 0;
      line = startLine;
      length = s.length;
      indent = [''];
      tokens = [];
      advance = function() {
        var delimiter, digit, l, now, prev, q, space, type, value;
        if (tokens.length) {
          return tokens.shift();
        }
        c = s[i];
        while (isSpace(c)) {
          ++i;
          c = s[i];
        }
        character = 1 + i - lastNewline;
        if (i >= length) {
          if (indent.length > 1) {
            indent.pop();
            return token('outdent');
          }
          return token('end');
        }
        if ((q = s.substr(i, 3)) === '|:=' || (q = s.substr(i, 2)) === ':=' || q === '|=' || q === '<-' || q === '<<' || q === '::') {
          i += q.length;
          return token(symbol[q], q);
        }
        switch (c) {
          case '\n':
          case '\r':
            while (true) {
              while (c === '\n' || c === '\r') {
                ++i;
                ++line;
                lastNewline = i;
                character = 1 + i - lastNewline;
                c = s[i];
              }
              space = '';
              while (true) {
                c = s[i];
                if (!isSpace(c)) {
                  break;
                }
                space += c;
                ++i;
              }
              if (!(c === '\n' || c === '\r')) {
                break;
              }
            }
            l = last(indent);
            now = space.length;
            prev = l.length;
            switch (false) {
              case now !== prev:
                if (space !== l) {
                  syntaxError('Inconsistent indentation');
                }
                return token('newline');
              case !(now < prev):
                while (now < prev) {
                  tokens.push(token('outdent'));
                  indent.pop();
                  l = last(indent);
                  prev = l.length;
                }
                if (c && space !== l) {
                  syntaxError('Inconsistent indentation');
                }
                return tokens.shift();
              case !(now > prev):
                if (l !== space.substr(0, prev)) {
                  syntaxError('Inconsistent indentation');
                }
                indent.push(space);
                return token('indent');
            }
            break;
          case '{':
          case '}':
          case '[':
          case ']':
          case ':':
          case '=':
          case ',':
          case '(':
          case ')':
            ++i;
            if (c === '=' && s[i] === '=') {
              --i;
            } else {
              return token(symbol[c], c);
            }
        }
        if (c === '-') {
          q = s[i + 1];
          if (q === '.' || isDigit(q)) {
            digit = true;
          }
        }
        if (digit || c === '.' || isDigit(c)) {
          value = '';
          start = i;
          if (c === '-') {
            value += c;
            ++i;
            c = s[i];
          }
          if (isDigit(c)) {
            while (true) {
              value += c;
              ++i;
              c = s[i];
              if (!isDigit(c)) {
                break;
              }
            }
          }
          b = false;
          if (c === '.') {
            value += c;
            ++i;
            c = s[i];
            if (!isDigit(c)) {
              --i;
              c = s[i];
              if (value !== '.') {
                return token('number', +value.substr(0, value.length - 1));
              }
              b = true;
            } else {
              while (true) {
                value += c;
                ++i;
                c = s[i];
                if (!isDigit(c)) {
                  break;
                }
              }
            }
          }
          if (!b) {
            if (c === 'e' || c === 'E') {
              value += c;
              ++i;
              c = s[i];
              if (c === '+' || c === '-') {
                value += c;
                ++i;
                c = s[i];
              }
              if (!isDigit(c)) {
                character = 1 + i - lastNewline;
                syntaxError('Expected digit');
              }
              while (true) {
                value += c;
                ++i;
                c = s[i];
                if (!isDigit(c)) {
                  break;
                }
              }
            }
            if (isIdentifier(c)) {
              i = start;
              c = s[i];
            } else {
              return token('number', +value);
            }
          }
        }
        if (c === '@' || c === '.' || isIdentifier(c)) {
          type = c === '@' ? 'formal parameter' : c === '.' ? 'symbol' : 'identifier';
          value = '';
          while (true) {
            value += c;
            ++i;
            c = s[i];
            if (!isIdentifier(c)) {
              break;
            }
          }
          if (value === 'true' || value === 'false') {
            return token('boolean', value === 'true');
          }
          if (-1 !== ['nil', 'unknown', 'class', 'module', 'try', 'catch', 'finally', 'if', 'then', 'else'].indexOf(value)) {
            return token(value, value);
          }
          if (type === 'symbol') {
            if (value.length === 1) {
              character = 1 + i - lastNewline;
              syntaxError('Expected identifier');
            }
            return token(type, value.substr(1));
          }
          return token(type, value);
        }
        if (c === '"' || c === "'") {
          delimiter = c;
          value = '';
          ++i;
          c = s[i];
          while (true) {
            if (c === delimiter) {
              break;
            }
            if (c === '\\') {
              ++i;
              switch (c = s[i]) {
                case '"':
                case "'":
                case '\\':
                  value += c;
                  break;
                case 'n':
                  value += '\n';
                  break;
                case 'r':
                  value += '\r';
                  break;
                case 't':
                  value += '\t';
                  break;
                default:
                  character = 1 + i - lastNewline;
                  syntaxError('Invalid escape sequence');
              }
              ++i;
              c = s[i];
              continue;
            }
            if (!c) {
              syntaxError('Unterminated string');
            }
            value += c;
            ++i;
            c = s[i];
          }
          ++i;
          return token('string', value);
        }
        return syntaxError("Unexpected " + c);
      };
      here = advance();
      next = advance();
      prev = null;
      return {
        here: function() {
          return here;
        },
        next: function() {
          return next;
        },
        advance: function() {
          prev = here;
          here = next;
          next = advance();
          return prev;
        },
        match: function() {
          var types;
          types = 1 <= arguments.length ? __slice.call(arguments, 0) : [];
          if (-1 !== types.indexOf(here.type)) {
            return this.advance();
          }
          if (prev && prev.type === 'outdent' && -1 !== types.indexOf('newline')) {
            return {
              type: 'newline',
              value: '',
              file: prev.file,
              line: prev.line,
              character: prev.character
            };
          }
          return null;
        },
        accept: function() {
          var values;
          values = 1 <= arguments.length ? __slice.call(arguments, 0) : [];
          if (-1 !== values.indexOf(here.value)) {
            return this.advance();
          }
          return null;
        },
        require: function() {
          var types;
          types = 1 <= arguments.length ? __slice.call(arguments, 0) : [];
          return this.match.apply(this, types) || parseError(here, "Expected " + (types.join(" or ")));
        },
        requireValue: function() {
          var values;
          values = 1 <= arguments.length ? __slice.call(arguments, 0) : [];
          return this.accept.apply(this, values) || parseError(here, "Expected '" + (values.join("', '")) + "'");
        }
      };
    };
  })();

  parse = (function() {
    var BINARY_OPERATORS, PREFIX_OPERATORS, Symbol, expression, pattern, sequence, symbols, value;
    symbols = {};
    BINARY_OPERATORS = {
      '^': -7,
      '*': 6,
      '/': 6,
      '%': 6,
      '+': 5,
      '-': 5,
      '>': 4,
      '<': 4,
      '>=': 4,
      '<=': 4,
      '==': 3,
      '!=': 3,
      'is': 3,
      'isnt': 3,
      'and': 2,
      'or': 2
    };
    PREFIX_OPERATORS = {
      'not': 2
    };
    Symbol = (function() {
      function Symbol() {}

      Symbol.prototype["null"] = function() {
        return parseError("Unexpected operator");
      };

      Symbol.prototype.left = function() {
        return parseError("Unexpected value");
      };

      return Symbol;

    })();
    parse = function(s, file, startLine) {
      var result, tokens;
      tokens = tokenizer(s, file, startLine);
      while (tokens.match('newline')) {}
      result = sequence(tokens);
      tokens.require('end');
      return result;
    };
    sequence = function(tokens) {
      var n, result, s;
      n = tokens.here();
      result = [];
      while (true) {
        s = expression(tokens, 0, true);
        if (!s) {
          break;
        }
        result.push(s);
        if (!tokens.match('newline')) {
          break;
        }
      }
      return {
        file: n.file,
        line: n.line,
        character: n.character,
        type: 'sequence',
        expressions: result
      };
    };
    expression = function(tokens, precedence, optional, noIndent) {
      var assignment, body, condition, cont, e, ep, falseBody, handler, name, op, operand, p, parent, result, start, symbol, t, trueBody, _class;
      if (precedence == null) {
        precedence = 0;
      }
      if (tokens.match('indent')) {
        result = sequence(tokens);
        tokens.require('outdent');
        return result;
      }
      if (start = tokens.match('class', 'module')) {
        if (name = tokens.match('identifier')) {
          while (true) {
            if (t = tokens.match('symbol')) {
              name = {
                type: 'application',
                file: t.file,
                line: t.line,
                character: t.character,
                funject: name,
                argument: t
              };
              continue;
            }
            break;
          }
        }
        if (tokens.accept('<')) {
          parent = expression(tokens, 0, false, true);
        }
        if (tokens.here().type !== 'indent') {
          parseError(tokens.here(), "Expected indent");
        }
        body = expression(tokens);
        result = {
          type: start.type,
          file: start.file,
          line: start.line,
          character: start.character,
          body: body,
          parent: parent
        };
        if (!name) {
          return result;
        }
        return {
          type: 'assignment',
          operator: 'strict assignment',
          file: start.file,
          line: start.line,
          character: start.character,
          left: name,
          right: result
        };
      }
      if (start = tokens.match('try')) {
        if (tokens.here().type !== 'indent') {
          parseError(tokens.here(), "Expected indent");
        }
        body = expression(tokens);
        tokens.require('catch');
        _class = tokens.require('identifier');
        name = tokens.match('identifier');
        if (tokens.here().type !== 'indent') {
          parseError(tokens.here(), "Expected indent");
        }
        handler = expression(tokens);
        result = {
          type: 'try',
          file: start.file,
          line: start.line,
          character: start.character,
          body: body,
          "class": _class,
          "catch": handler
        };
        if (name) {
          result.name = name;
        }
        if (tokens.match('finally')) {
          result["finally"] = expression(tokens);
        }
        return result;
      }
      if (start = tokens.match('if')) {
        condition = expression(tokens, 0, false, true);
        if (tokens.here().type !== 'indent') {
          tokens.require('then');
        }
        trueBody = expression(tokens);
        if (tokens.match('else')) {
          if (tokens.here().type !== 'indent' && !tokens.here().type === 'if') {
            parseError(tokens.here(), "Expected indent");
          }
          falseBody = expression(tokens);
        }
        return {
          type: 'if',
          file: start.file,
          line: start.line,
          character: start.character,
          condition: condition,
          trueBody: trueBody,
          falseBody: falseBody
        };
      }
      e = value(tokens);
      if (!e) {
        if (optional) {
          return null;
        } else {
          parseError(tokens.here(), "Expected expression");
        }
      }
      while (true) {
        if (t = tokens.match('symbol')) {
          e = {
            type: 'application',
            file: t.file,
            line: t.line,
            character: t.character,
            funject: e,
            argument: t
          };
          continue;
        }
        if (tokens.here().type === 'list start') {
          t = tokens.here();
          e = {
            type: 'application',
            file: t.file,
            line: t.line,
            character: t.character,
            funject: e,
            argument: value(tokens)
          };
          continue;
        }
        if (precedence < 7) {
          if (tokens.here().type === 'identifier') {
            cont = false;
            for (op in BINARY_OPERATORS) {
              p = BINARY_OPERATORS[op];
              if (precedence < Math.abs(p) && (t = tokens.accept(op))) {
                e = {
                  type: 'application',
                  file: t.file,
                  line: t.line,
                  character: t.character,
                  funject: e,
                  argument: t
                };
                ep = p < 0 ? -1 - p : p;
                if (operand = expression(tokens, ep, true, noIndent)) {
                  e = op === 'and' || op === 'or' ? {
                    type: op,
                    file: t.file,
                    line: t.line,
                    character: t.character,
                    left: e.funject,
                    right: operand
                  } : {
                    type: 'application',
                    file: t.file,
                    line: t.line,
                    character: t.character,
                    funject: e,
                    argument: operand
                  };
                }
                cont = true;
                break;
              }
            }
            if (cont) {
              continue;
            }
          }
          if ('identifier' === tokens.here().type && !BINARY_OPERATORS[tokens.here().value] || -1 !== ['formal parameter', 'string', 'number', 'boolean', 'nil', 'unknown', 'funject start', 'group start', 'list start'].indexOf(tokens.here().type)) {
            t = tokens.here();
            e = {
              type: 'application',
              file: t.file,
              line: t.line,
              character: t.character,
              funject: e,
              argument: expression(tokens, 7, false, noIndent)
            };
            continue;
          }
          if (t = tokens.match('prototypal application')) {
            symbol = tokens.require('identifier');
            symbol.type = 'symbol';
            e = {
              type: 'application',
              file: t.file,
              line: symbol.line,
              character: symbol.character,
              funject: {
                type: 'application',
                file: t.file,
                line: t.line,
                character: t.character,
                funject: e,
                argument: {
                  type: 'symbol',
                  file: t.file,
                  line: t.line,
                  character: t.character,
                  value: 'instance'
                }
              },
              argument: symbol
            };
            continue;
          }
          if (!noIndent && 'indent' === tokens.here().type) {
            t = tokens.here();
            e = {
              type: 'application',
              file: t.file,
              line: t.line,
              character: t.character,
              funject: e,
              argument: expression(tokens)
            };
            break;
          }
        }
        break;
      }
      if (precedence === 0 && (assignment = tokens.match('strict assignment', 'lazy assignment', 'reset strict assignment', 'reset lazy assignment', 'inverse assignment', 'inheritance assignment'))) {
        if (assignment.type !== 'inheritance assignment' && assignment.type !== 'inverse assignment' && e.type !== 'identifier' && (e.type !== 'application' || assignment.type !== 'strict assignment' && assignment.type !== 'lazy assignment')) {
          parseError(assignment, 'Invalid left-hand side of assignment');
        }
        e = {
          type: 'assignment',
          operator: assignment.type,
          file: assignment.file,
          line: assignment.line,
          character: assignment.character,
          left: e,
          right: expression(tokens, 0, false, noIndent)
        };
      }
      return e;
    };
    value = function(tokens) {
      var e, patterns, t, values;
      if (tokens.match('group start')) {
        e = expression(tokens);
        tokens.require('group end');
        return e;
      }
      if (t = tokens.match('list start')) {
        values = [];
        if (!tokens.match('list end')) {
          while (true) {
            values.push(expression(tokens));
            if (tokens.match('list end')) {
              break;
            }
            tokens.require('list delimiter');
          }
        }
        return {
          file: t.file,
          line: t.line,
          character: t.character,
          type: 'list',
          values: values
        };
      }
      if (t = tokens.match('funject start')) {
        patterns = [];
        if (!tokens.match('funject end')) {
          if (tokens.match('indent')) {
            while (true) {
              patterns.push(pattern(tokens));
              if (tokens.match('outdent')) {
                break;
              }
              tokens.require('newline');
            }
            tokens.require('funject end');
          } else {
            patterns.push(pattern(tokens));
            tokens.require('funject end');
          }
        }
        return {
          file: t.file,
          line: t.line,
          character: t.character,
          type: 'funject',
          patterns: patterns
        };
      }
      if (tokens.here().type === 'identifier' && Object.prototype.hasOwnProperty.call(PREFIX_OPERATORS, tokens.here().value)) {
        t = tokens.advance();
        return {
          type: 'application',
          file: t.file,
          line: t.line,
          character: t.character,
          funject: expression(tokens, PREFIX_OPERATORS[t.value]),
          argument: {
            type: 'symbol',
            file: t.file,
            line: t.line,
            character: t.character,
            value: 'not'
          }
        };
      }
      if (t = tokens.match('identifier', 'formal parameter', 'symbol', 'string', 'number', 'boolean', 'nil', 'unknown')) {
        return t;
      }
    };
    pattern = function(tokens) {
      var match, result;
      match = expression(tokens);
      tokens.require('pattern match');
      result = expression(tokens);
      return {
        pattern: match,
        value: result
      };
    };
    return parse;
  })();

  parseForRacket = function(s) {
    var transform;
    transform = function(n) {
      var p, x;
      switch (n.type) {
        case 'number':
          return ['Token-number', n.value];
        case 'symbol':
          return ['Token-symbol', n.value];
        case 'string':
          return ['Token-string', n.value];
        case 'boolean':
          return ['Token-boolean', n.value];
        case 'nil':
          return ['Token-nil'];
        case 'unknown':
          return ['Token-unknown'];
        case 'identifier':
          return ['Token-identifier', n.value];
        case 'formal parameter':
          return ['Token-parameter', n.value];
        case 'list':
          return [
            'Token-list', (function() {
              var _i, _len, _ref, _results;
              _ref = n.values;
              _results = [];
              for (_i = 0, _len = _ref.length; _i < _len; _i++) {
                x = _ref[_i];
                _results.push(transform(x));
              }
              return _results;
            })()
          ];
        case 'funject':
          return [
            'Token-funject', (function() {
              var _i, _len, _ref, _results;
              _ref = n.patterns;
              _results = [];
              for (_i = 0, _len = _ref.length; _i < _len; _i++) {
                p = _ref[_i];
                _results.push([transform(p.pattern), transform(p.value)]);
              }
              return _results;
            })()
          ];
        case 'sequence':
          return [
            'Token-sequence', (function() {
              var _i, _len, _ref, _results;
              _ref = n.expressions;
              _results = [];
              for (_i = 0, _len = _ref.length; _i < _len; _i++) {
                x = _ref[_i];
                _results.push(transform(x));
              }
              return _results;
            })()
          ];
        case 'application':
          return ['Token-invocation', transform(n.funject), transform(n.argument)];
        case 'or':
        case 'and':
          return ['Token-' + n.type, transform(n.left), transform(n.right)];
        case 'class':
        case 'module':
          if (n.parent) {
            return ['Token-' + n.type, transform(n.parent), transform(n.body)];
          } else {
            return ['Token-' + n.type, transform(n.body)];
          }
          break;
        case 'try':
          if (n["finally"]) {
            return ['Token-try', transform(n.body), transform(n["catch"]), transform(n["finally"])];
          } else {
            return ['Token-try', transform(n.body), transform(n["catch"])];
          }
          break;
        case 'assignment':
          switch (n.operator) {
            case 'strict assignment':
              if (n.left.type === 'application') {
                return ['Token-funject-strict-assignment', transform(n.left), transform(n.right)];
              } else {
                return ['Token-strict-assignment', transform(n.left), transform(n.right)];
              }
              break;
            case 'lazy assignment':
              if (n.left.type === 'application') {
                return ['Token-funject-lazy-assignment', transform(n.left), transform(n.right)];
              } else {
                return ['Token-lazy-assignment', transform(n.left), transform(n.right)];
              }
              break;
            case 'reset strict assignment':
              return ['Token-reset-strict-assignment', transform(n.left), transform(n.right)];
            case 'reset lazy assignment':
              return ['Token-reset-lazy-assignment', transform(n.left), transform(n.right)];
            case 'inheritance assignment':
              return ['Token-funject-inheritance', transform(n.left), transform(n.right)];
            case 'inverse assignment':
              return ['Token-inverse-definition', transform(n.left), transform(n.right)];
          }
      }
    };
    return transform(parse(s));
  };

  printTokens = function(s) {
    var indent, t, tokens, _results;
    tokens = tokenizer(s);
    indent = '';
    _results = [];
    while (true) {
      t = tokens.advance();
      if (t.type === 'end') {
        break;
      }
      switch (t.type) {
        case 'indent':
          console.log("" + indent + "{");
          _results.push(indent += '    ');
          break;
        case 'outdent':
          indent = indent.substr(4);
          _results.push(console.log("" + indent + "}"));
          break;
        case 'newline':
          _results.push(console.log("" + indent + ";"));
          break;
        default:
          _results.push(console.log("" + indent + "(:" + t.line + ":" + t.character + ") <" + t.type + "> " + t.value));
      }
    }
    return _results;
  };

  printTree = function(s) {
    var print;
    print = function(n, indent) {
      var name, v, _i, _j, _k, _len, _len1, _len2, _ref, _ref1, _ref2, _results, _results1, _results2;
      if (indent == null) {
        indent = '';
      }
      if (n instanceof Array) {
        if (typeof n[0] === 'string') {
          name = n[0].replace(/^Token-/, '');
          if (n.length === 2 && typeof n[1] !== 'object') {
            return console.log("" + indent + name + ": " + n[1]);
          } else {
            console.log("" + indent + name);
            if (n[1] && typeof n[1][0] !== 'string') {
              _ref = n[1];
              _results = [];
              for (_i = 0, _len = _ref.length; _i < _len; _i++) {
                v = _ref[_i];
                _results.push(print(v, indent + '    '));
              }
              return _results;
            } else {
              _ref1 = n.slice(1);
              _results1 = [];
              for (_j = 0, _len1 = _ref1.length; _j < _len1; _j++) {
                v = _ref1[_j];
                _results1.push(print(v, indent + '    '));
              }
              return _results1;
            }
          }
        } else {
          console.log("" + indent + "pattern:");
          _ref2 = n.slice(0);
          _results2 = [];
          for (_k = 0, _len2 = _ref2.length; _k < _len2; _k++) {
            v = _ref2[_k];
            _results2.push(print(v, indent + '    '));
          }
          return _results2;
        }
      } else {
        return console.log(indent + n);
      }
    };
    return print(parseForRacket(s));
  };

  stringify = function(n) {};

  if (typeof module !== "undefined" && module !== null) {
    _exports = exports;
    if (!module.parent) {
      expression = null;
      racket = false;
      i = 2;
      tokens = false;
      verbose = false;
      argc = process.argv.length;
      while (i < argc) {
        switch (arg = process.argv[i++]) {
          case '-r':
            racket = true;
            break;
          case '-t':
            tokens = true;
            break;
          case '-v':
            verbose = true;
            break;
          case '-e':
            expression = process.argv[i++];
            break;
          default:
            expression = '' + require('fs').readFileSync(arg);
            break;
        }
      }
      if (i < argc || (expression == null)) {
        console.error('Usage: coffee parser.coffee [ -r | -t | -v ] [ <filename> | -e <expression> ]');
        return;
      }
      try {
        if (tokens) {
          printTokens(expression);
        } else if (verbose || racket) {
          p = racket ? parseForRacket : parse;
          console.log(JSON.stringify(p(expression), void 0, 2));
        } else {
          printTree(expression);
        }
      } catch (_error) {
        e = _error;
        if (e instanceof SyntaxError) {
          console.error(e.message);
        } else {
          throw e;
        }
      }
    }
  } else {
    (this.Yak != null ? this.Yak : this.Yak = {}).parser = _exports = {};
  }

  _ref = {
    IDENTIFIER_CHARS: IDENTIFIER_CHARS,
    parse: parse,
    stringify: stringify,
    tokenizer: tokenizer,
    parseForRacket: parseForRacket
  };
  for (k in _ref) {
    v = _ref[k];
    _exports[k] = v;
  }

}).call(this);