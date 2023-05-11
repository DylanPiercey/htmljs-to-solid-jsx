import * as t from "@babel/types";
import generate from "@babel/generator";
import { parseExpression, parse as parseStatement } from "@babel/parser";
import {
  type Node,
  type Range,
  type Ranges,
  parse,
  NodeType,
} from "@marko/language-tools";
const solidWebSource = "solid-js/web";
const defaultAttrName = "value";
const blockReg = /(?<=\s*){/y;
const emptyArr = [] as any[];

// TODO
// tag var
// dynamic tag

export function transform(code: string, filename: string) {
  const { program, read, positionAt, locationAt } = parse(code);
  const solidImports = new Map<string, t.Identifier>();
  const babelProgram: t.Program = {
    type: "Program",
    sourceType: "module",
    sourceFile: filename,
    body: [],
    directives: emptyArr,
  };
  let uid = 0;

  for (const node of program.static) {
    let statements: t.Statement[] = [];
    if (node.type === NodeType.Static) {
      let start = node.start + "static ".length;
      let end = node.end;
      blockReg.lastIndex = start;

      if (blockReg.test(code)) {
        start = blockReg.lastIndex;
        end--;
      }

      statements = parseStatementAt(start, read({ start, end }));
    } else {
      statements = parseStatementAt(node.start, read(node));
    }

    for (const node of statements) {
      babelProgram.body.push(node);
    }
  }

  let statements: t.Statement[] = [];
  let children: t.JSXFragment["children"] = [];
  let attrs:
    | (
        | t.JSXAttribute
        | t.JSXSpreadAttribute
        | t.ObjectProperty
        | t.SpreadElement
      )[]
    | undefined;

  for (const node of program.body) transform(node);

  babelProgram.body.push(
    buildExportDefaultDeclaration(
      buildArrowFunctionExpression(
        [buildIdentifier("props")],
        statements.length
          ? buildBlockStatement(
              statements.concat(
                buildReturnStatement(firstElementOrFragment(children))
              )
            )
          : firstElementOrFragment(children)
      )
    )
  );

  if (solidImports.size) {
    let solidImport = findSolidImport(babelProgram);
    if (!solidImport) {
      solidImport = buildImportDeclaration(
        [],
        buildStringLiteral(solidWebSource)
      );
      babelProgram.body.unshift(solidImport);
    }

    for (const [imported, local] of solidImports) {
      solidImport.specifiers.push(
        buildImportSpecifier(local, buildIdentifier(imported))
      );
    }
  }

  return (generate as any as { default: typeof generate }).default(
    babelProgram,
    {
      comments: true,
      compact: false,
      concise: false,
      filename,
      minified: false,
      sourceFileName: filename,
      sourceMaps: true,
    },
    code
  );

  function transform(node: Node.ChildNode) {
    switch (node.type) {
      case NodeType.Text:
        children.push(withLoc(buildJSXText(read(node)), node));
        break;
      case NodeType.Placeholder: {
        const value = parseExpressionAt(node.value.start, read(node.value));
        children.push(
          node.escape
            ? buildJSXExpressionContainer(value)
            : buildJSXElement(
                buildJSXOpenElement(
                  buildJSXIdentifier("div"),
                  [
                    buildJSXAttribute(
                      buildJSXIdentifier("innerHTML"),
                      wrapJSXExpressionIfNeeded(value)
                    ),
                  ],
                  true
                ),
                undefined,
                emptyArr,
                true
              )
        );
        break;
      }
      case NodeType.Scriptlet:
        for (const stmt of parseStatementAt(
          node.value.start,
          read(node.value)
        )) {
          statements.push(stmt);
        }
        break;
      case NodeType.AttrTag: {
        const nameText = read(node.name);
        const curProps: (t.ObjectProperty | t.SpreadElement)[] = [];
        let params: t.ArrowFunctionExpression["params"] | undefined;
        let body:
          | t.JSXFragment
          | t.JSXElement
          | t.UnaryExpression
          | t.BlockStatement
          | undefined;

        if (node.shorthandId) {
          curProps.push(
            buildObjectProperty(
              buildIdentifier("id"),
              parseTemplateLiteralAt(node.shorthandId)
            )
          );
        }

        if (node.shorthandClassNames) {
          const quasis: t.TemplateElement[] = [];
          const expressions: t.Expression[] = [];
          let buf = "";
          let sep = "";

          for (const className of node.shorthandClassNames) {
            const len = className.expressions.length;
            buf += sep + read(className.quasis[0]);
            sep = " ";

            for (let i = 0; i < len; i++) {
              const expr = className.expressions[i].value;
              expressions.push(
                buildLogicalExpresion(
                  "??",
                  parseExpressionAt(expr.start, read(expr)),
                  buildStringLiteral("")
                )
              );
              quasis.push(buildTemplateElement(buf, false));
              buf = read(className.quasis[i + 1]);
            }
          }

          if (quasis.length) {
            quasis.push(buildTemplateElement(buf, true));

            curProps.push(
              buildObjectProperty(
                buildIdentifier("class"),
                buildTemplateLiteral(quasis, expressions)
              )
            );
          } else {
            curProps.push(
              buildObjectProperty(
                buildIdentifier("class"),
                buildStringLiteral(buf)
              )
            );
          }
        }

        if (node.attrs) {
          for (const prop of node.attrs) {
            switch (prop.type) {
              case NodeType.AttrSpread:
                curProps.push(
                  buildSpreadElement(
                    parseExpressionAt(prop.value.start, read(prop.value))
                  )
                );
                break;
              case NodeType.AttrNamed: {
                const propName = read(prop.name) || defaultAttrName;
                const propNameId = withLoc(
                  buildStringLiteral(propName),
                  prop.name
                );
                if (prop.value) {
                  if (prop.value.type === NodeType.AttrValue) {
                    const value = parseExpressionAt(
                      prop.value.value.start,
                      read(prop.value.value)
                    );
                    curProps.push(buildObjectProperty(propNameId, value));

                    if (prop.value.bound) {
                      if (
                        value.type !== "CallExpression" ||
                        value.callee.type !== "Identifier" ||
                        value.arguments.length
                      ) {
                        throw Object.assign(
                          new Error("Unsupported bound value."),
                          { loc: value.loc }
                        );
                      }

                      const { name } = value.callee;
                      curProps.push(
                        buildObjectProperty(
                          buildIdentifier(`${propName}Change`),
                          buildArrowFunctionExpression(
                            [buildIdentifier("v")],
                            buildBlockStatement([
                              buildExpressionStatement(
                                buildCallExpression(
                                  buildIdentifier(
                                    `set${name[0].toUpperCase()}${name.slice(
                                      1
                                    )}`
                                  ),
                                  [buildIdentifier("v")]
                                )
                              ),
                            ])
                          )
                        )
                      );
                    }
                  } else {
                    curProps.push(
                      buildObjectProperty(
                        propNameId,
                        buildArrowFunctionExpression(
                          parseExpressionAt<t.ArrowFunctionExpression>(
                            prop.value.params.start,
                            `${read(prop.value.params)}=>{}`
                          ).params,
                          parseExpressionAt<t.ArrowFunctionExpression>(
                            prop.value.body.start - 4,
                            `()=>${read(prop.value.body)}`
                          ).body
                        )
                      )
                    );
                  }
                }
                break;
              }
            }
          }
        }

        if (node.body) {
          const curChildren: typeof children = [];
          const curStatements: typeof statements = [];
          const parentAttrs = attrs;
          const parentChildren = children;
          const parentStatements = statements;
          attrs = curProps;
          children = curChildren;
          statements = curStatements;

          for (const child of node.body) {
            transform(child);
          }

          if (children.length || statements.length) {
            if (node.params) {
              params = parseExpressionAt<t.ArrowFunctionExpression>(
                node.params.start,
                `(${read({
                  start: node.params.start + 1,
                  end: node.params.end - 1,
                })})=>{}`
              ).params;
            }

            if (statements.length) {
              if (children.length) {
                statements.push(
                  buildReturnStatement(firstElementOrFragment(children))
                );
              }

              body = buildBlockStatement(statements);
            } else {
              body = firstElementOrFragment(children);
            }
          }

          attrs = parentAttrs;
          children = parentChildren;
          statements = parentStatements;
        }

        const attrTagProps = buildObjectExpression(curProps);
        const attrTagChildren =
          body &&
          (params
            ? buildArrowFunctionExpression(params, body)
            : body.type === "JSXElement" || body.type === "JSXFragment"
            ? body
            : buildCallExpression(
                buildArrowFunctionExpression(emptyArr, body),
                emptyArr
              ));
        attrs?.push(
          buildJSXAttribute(
            buildJSXIdentifier(nameText.slice(1)),
            buildJSXExpressionContainer(
              attrTagChildren
                ? attrTagProps.properties.length
                  ? buildCallExpression(
                      buildMemberExpression(
                        buildIdentifier("Object"),
                        buildIdentifier("assign")
                      ),
                      [attrTagChildren, attrTagProps]
                    )
                  : attrTagChildren
                : attrTagProps
            )
          )
        );

        break;
      }
      case NodeType.Tag: {
        const curAttrs: (t.JSXAttribute | t.JSXSpreadAttribute)[] = [];
        const { nameText } = node;
        let nameId: t.JSXIdentifier;
        let params: t.ArrowFunctionExpression["params"] | undefined;
        let body:
          | t.JSXFragment
          | t.JSXElement
          | t.UnaryExpression
          | t.BlockStatement
          | undefined;

        if (nameText) {
          nameId = withLoc(buildJSXIdentifier(nameText), node.name);
        } else {
          nameId = buildJSXIdentifier(getSolidImport("Dynamic").name);
          curAttrs.push(
            buildJSXAttribute(
              buildJSXIdentifier("component"),
              buildJSXExpressionContainer(parseTemplateLiteralAt(node.name))
            )
          );
        }

        if (node.shorthandId) {
          curAttrs.push(
            buildJSXAttribute(
              buildJSXIdentifier("id"),
              buildJSXExpressionContainer(
                parseTemplateLiteralAt(node.shorthandId)
              )
            )
          );
        }

        if (node.shorthandClassNames) {
          const quasis: t.TemplateElement[] = [];
          const expressions: t.Expression[] = [];
          let buf = "";
          let sep = "";

          for (const className of node.shorthandClassNames) {
            const len = className.expressions.length;
            buf += sep + read(className.quasis[0]);
            sep = " ";

            for (let i = 0; i < len; i++) {
              const expr = className.expressions[i].value;
              expressions.push(
                buildLogicalExpresion(
                  "??",
                  parseExpressionAt(expr.start, read(expr)),
                  buildStringLiteral("")
                )
              );
              quasis.push(buildTemplateElement(buf, false));
              buf = read(className.quasis[i + 1]);
            }
          }

          if (quasis.length) {
            quasis.push(buildTemplateElement(buf, true));

            curAttrs.push(
              buildJSXAttribute(
                buildJSXIdentifier("class"),
                buildJSXExpressionContainer(
                  buildTemplateLiteral(quasis, expressions)
                )
              )
            );
          } else {
            curAttrs.push(
              buildJSXAttribute(
                buildJSXIdentifier("class"),
                buildStringLiteral(buf)
              )
            );
          }
        }

        if (node.attrs) {
          for (const attr of node.attrs) {
            switch (attr.type) {
              case NodeType.AttrSpread:
                curAttrs.push(
                  buildJSXSpreadAttribute(
                    parseExpressionAt(attr.value.start, read(attr.value))
                  )
                );
                break;
              case NodeType.AttrNamed: {
                const attrName = read(attr.name) || defaultAttrName;
                const attrNameId = withLoc(
                  buildJSXIdentifier(attrName),
                  attr.name
                );
                if (attr.value) {
                  if (attr.value.type === NodeType.AttrValue) {
                    const value = parseExpressionAt(
                      attr.value.value.start,
                      read(attr.value.value)
                    );
                    curAttrs.push(
                      buildJSXAttribute(
                        attrNameId,
                        wrapJSXExpressionIfNeeded(value)
                      )
                    );

                    if (attr.value.bound) {
                      if (
                        value.type !== "CallExpression" ||
                        value.callee.type !== "Identifier" ||
                        value.arguments.length
                      ) {
                        throw Object.assign(
                          new Error("Unsupported bound value."),
                          { loc: value.loc }
                        );
                      }

                      const { name } = value.callee;
                      const newValueId = buildIdentifier("v");
                      curAttrs.push(
                        buildJSXAttribute(
                          buildJSXIdentifier(`${attrName}Change`),
                          buildJSXExpressionContainer(
                            buildArrowFunctionExpression(
                              [newValueId],
                              buildBlockStatement([
                                buildExpressionStatement(
                                  buildCallExpression(
                                    buildIdentifier(
                                      `set${name[0].toUpperCase()}${name.slice(
                                        1
                                      )}`
                                    ),
                                    [newValueId]
                                  )
                                ),
                              ])
                            )
                          )
                        )
                      );
                    }
                  } else {
                    curAttrs.push(
                      buildJSXAttribute(
                        attrNameId,
                        buildJSXExpressionContainer(
                          buildArrowFunctionExpression(
                            parseExpressionAt<t.ArrowFunctionExpression>(
                              attr.value.params.start,
                              `${read(attr.value.params)}=>{}`
                            ).params,
                            parseExpressionAt<t.ArrowFunctionExpression>(
                              attr.value.body.start - 4,
                              `()=>${read(attr.value.body)}`
                            ).body
                          )
                        )
                      )
                    );
                  }
                }
                break;
              }
            }
          }
        }

        if (node.body) {
          const curChildren: typeof children = [];
          const curStatements: typeof statements = [];
          const parentAttrs = attrs;
          const parentChildren = children;
          const parentStatements = statements;
          attrs = curAttrs;
          children = curChildren;
          statements = curStatements;

          for (const child of node.body) {
            transform(child);
          }

          if (children.length || statements.length) {
            if (node.params) {
              params = parseExpressionAt<t.ArrowFunctionExpression>(
                node.params.start,
                `(${read({
                  start: node.params.start + 1,
                  end: node.params.end - 1,
                })})=>{}`
              ).params;
            }

            if (statements.length) {
              if (children.length) {
                statements.push(
                  buildReturnStatement(firstElementOrFragment(children))
                );
              }

              body = buildBlockStatement(statements);
            } else {
              body = firstElementOrFragment(children);
            }
          }

          attrs = parentAttrs;
          children = parentChildren;
          statements = parentStatements;
        }

        const selfClosed = !body;

        children.push(
          withLoc(
            buildJSXElement(
              buildJSXOpenElement(nameId, curAttrs, selfClosed),
              selfClosed ? undefined : buildJSXClosingElement(nameId),
              body
                ? params
                  ? [
                      buildJSXExpressionContainer(
                        buildArrowFunctionExpression(params, body)
                      ),
                    ]
                  : body.type === "JSXElement"
                  ? [body]
                  : body.type === "JSXFragment"
                  ? body.children
                  : [
                      buildJSXExpressionContainer(
                        buildCallExpression(
                          buildArrowFunctionExpression(emptyArr, body),
                          emptyArr
                        )
                      ),
                    ]
                : emptyArr,
              selfClosed
            ),
            node
          )
        );

        break;
      }
    }
  }

  function parseStatementAt(offset: number, src: string): t.Statement[] {
    const { line, character } = positionAt(offset);
    return parseStatement(src, {
      sourceFilename: filename,
      sourceType: "module",
      startColumn: character,
      startLine: line + 1,
      strictMode: true,
    }).program.body;
  }

  function parseExpressionAt<T = t.Expression>(offset: number, src: string): T {
    let { line, character } = positionAt(offset);
    return parseExpression(src, {
      sourceFilename: filename,
      sourceType: "module",
      startColumn: character,
      startLine: line + 1,
      strictMode: true,
    }) as unknown as T;
  }

  function parseTemplateLiteralAt(node: Ranges.Template) {
    const len = node.expressions.length;
    switch (len) {
      case 0:
        return withLoc(
          buildStringLiteral(read(node.quasis[0])),
          node.quasis[0]
        );
      case 1:
        if (isEmpty(node.quasis[0]) && isEmpty(node.quasis[1])) {
          return parseExpressionAt(
            node.expressions[0].value.start,
            read(node.expressions[0].value)
          );
        }
        break;
    }

    const expressions: t.Expression[] = new Array(len);
    const quasis: t.TemplateElement[] = new Array(len + 1);

    for (let i = 0; i < len; i++) {
      const expr = node.expressions[i].value;
      expressions[i] = parseExpressionAt(expr.start, read(expr));
      quasis[i] = buildTemplateElement(read(node.quasis[i]), false);
    }

    quasis[len] = buildTemplateElement(read(node.quasis[len]), true);

    return buildTemplateLiteral(quasis, expressions);
  }

  function withLoc<T extends t.Node>(node: T, range: Range): T {
    const { start, end } = locationAt(range);
    node.start = range.start;
    node.end = range.end;
    node.loc = {
      start: { line: start.line + 1, column: start.character },
      end: {
        line: end.line + 1,
        column: end.character,
      },
    };
    return node;
  }

  function getSolidImport(imported: string) {
    let local = solidImports.get(imported);
    if (!local) {
      local = buildUid(imported);
      solidImports.set(imported, local);
    }

    return local;
  }

  function buildUid(hint: string) {
    return buildIdentifier(
      `$${uid++}${hint.replace(/[^a-zA-Z90-9_$]+/g, "_").replace(/^_|_$/, "")}`
    );
  }
}

function firstElementOrFragment(
  children: t.JSXFragment["children"]
): t.JSXElement | t.JSXFragment | ReturnType<typeof buildUndefined> {
  if (children.length === 0) {
    return buildUndefined();
  }

  if (children.length === 1 && children[0].type === "JSXElement") {
    return children[0];
  }

  return {
    type: "JSXFragment",
    openingFragment: {
      type: "JSXOpeningFragment",
    },
    closingFragment: {
      type: "JSXClosingFragment",
    },
    children,
  };
}

function wrapJSXExpressionIfNeeded(node: t.Expression) {
  return node.type === "StringLiteral"
    ? node
    : buildJSXExpressionContainer(node);
}

function buildIdentifier(name: string): t.Identifier {
  return {
    type: "Identifier",
    name,
  };
}

function buildMemberExpression(
  object: t.MemberExpression["object"],
  property: t.MemberExpression["property"]
): t.MemberExpression {
  return {
    type: "MemberExpression",
    object,
    property,
    computed: false,
  };
}

function buildStringLiteral(value: string): t.StringLiteral {
  return {
    type: "StringLiteral",
    value,
  };
}

function buildTemplateLiteral(
  quasis: t.TemplateLiteral["quasis"],
  expressions: t.TemplateLiteral["expressions"]
): t.TemplateLiteral {
  return {
    type: "TemplateLiteral",
    quasis,
    expressions,
  };
}

function buildTemplateElement(raw: string, tail: boolean): t.TemplateElement {
  return {
    type: "TemplateElement",
    value: {
      raw,
      cooked: "",
    },
    tail,
  };
}

function buildUndefined() {
  return {
    type: "UnaryExpression",
    operator: "void",
    argument: {
      type: "NumericLiteral",
      value: 0,
    },
    prefix: true,
  } as const;
}

function buildBlockStatement(body: t.BlockStatement["body"]): t.BlockStatement {
  return {
    type: "BlockStatement",
    body,
    directives: emptyArr,
  };
}

function buildExpressionStatement(
  expression: t.ExpressionStatement["expression"]
): t.ExpressionStatement {
  return {
    type: "ExpressionStatement",
    expression,
  };
}

function buildLogicalExpresion(
  operator: t.LogicalExpression["operator"],
  left: t.LogicalExpression["left"],
  right: t.LogicalExpression["right"]
): t.LogicalExpression {
  return {
    type: "LogicalExpression",
    operator,
    left,
    right,
  };
}

function buildCallExpression(
  callee: t.CallExpression["callee"],
  args: t.CallExpression["arguments"]
): t.CallExpression {
  return {
    type: "CallExpression",
    callee,
    arguments: args,
  };
}

function buildArrowFunctionExpression(
  params: t.ArrowFunctionExpression["params"],
  body: t.ArrowFunctionExpression["body"]
): t.ArrowFunctionExpression {
  return {
    type: "ArrowFunctionExpression",
    params,
    body,
    async: false,
    expression: false,
  };
}

function buildVariableDeclaration(
  kind: t.VariableDeclaration["kind"],
  declarations: t.VariableDeclaration["declarations"]
): t.VariableDeclaration {
  return {
    type: "VariableDeclaration",
    kind,
    declarations,
  };
}

function buildVariableDeclarator(
  id: t.VariableDeclarator["id"],
  init: t.VariableDeclarator["init"]
): t.VariableDeclarator {
  return {
    type: "VariableDeclarator",
    id,
    init,
  };
}

function buildObjectExpression(
  properties: t.ObjectExpression["properties"]
): t.ObjectExpression {
  return {
    type: "ObjectExpression",
    properties,
  };
}

function buildObjectProperty(
  key: t.ObjectProperty["key"],
  value: t.ObjectProperty["value"]
): t.ObjectProperty {
  return {
    type: "ObjectProperty",
    key,
    value,
    computed: false,
    shorthand: false,
    decorators: emptyArr,
  };
}

function buildSpreadElement(
  argument: t.SpreadElement["argument"]
): t.SpreadElement {
  return {
    type: "SpreadElement",
    argument,
  };
}

function buildReturnStatement(
  argument: t.ReturnStatement["argument"]
): t.ReturnStatement {
  return {
    type: "ReturnStatement",
    argument,
  };
}

function buildImportDeclaration(
  specifiers: t.ImportDeclaration["specifiers"],
  source: t.ImportDeclaration["source"]
): t.ImportDeclaration {
  return {
    type: "ImportDeclaration",
    specifiers,
    source,
  };
}

function buildImportSpecifier(
  local: t.ImportSpecifier["local"],
  imported: t.ImportSpecifier["imported"]
): t.ImportSpecifier {
  return {
    type: "ImportSpecifier",
    local,
    imported,
  };
}

function buildExportDefaultDeclaration(
  declaration: t.ExportDefaultDeclaration["declaration"]
): t.ExportDefaultDeclaration {
  return {
    type: "ExportDefaultDeclaration",
    declaration,
  };
}

function buildJSXIdentifier(name: string): t.JSXIdentifier {
  return {
    type: "JSXIdentifier",
    name,
  };
}

function buildJSXExpressionContainer(
  expression: t.JSXExpressionContainer["expression"]
): t.JSXExpressionContainer {
  return {
    type: "JSXExpressionContainer",
    expression,
  };
}

function buildJSXText(value: t.JSXText["value"]): t.JSXText {
  return {
    type: "JSXText",
    value,
  };
}

function buildJSXAttribute(
  name: t.JSXAttribute["name"],
  value: t.JSXAttribute["value"]
): t.JSXAttribute {
  return {
    type: "JSXAttribute",
    name,
    value,
  };
}

function buildJSXSpreadAttribute(
  argument: t.JSXSpreadAttribute["argument"]
): t.JSXSpreadAttribute {
  return {
    type: "JSXSpreadAttribute",
    argument,
  };
}

function buildJSXOpenElement(
  name: t.JSXOpeningElement["name"],
  attributes: t.JSXOpeningElement["attributes"],
  selfClosing: t.JSXOpeningElement["selfClosing"]
): t.JSXOpeningElement {
  return {
    type: "JSXOpeningElement",
    name,
    attributes,
    selfClosing,
  };
}

function buildJSXClosingElement(
  name: t.JSXClosingElement["name"]
): t.JSXClosingElement {
  return {
    type: "JSXClosingElement",
    name,
  };
}

function buildJSXElement(
  openingElement: t.JSXElement["openingElement"],
  closingElement: t.JSXElement["closingElement"],
  children: t.JSXElement["children"],
  selfClosing: t.JSXElement["selfClosing"]
): t.JSXElement {
  return {
    type: "JSXElement",
    openingElement,
    closingElement,
    children,
    selfClosing,
  };
}

function findSolidImport(program: t.Program) {
  for (const node of program.body) {
    if (
      node.type === "ImportDeclaration" &&
      node.source.value === solidWebSource
    ) {
      return node;
    }
  }
}

function isEmpty(range: Range) {
  return range.start === range.end;
}
