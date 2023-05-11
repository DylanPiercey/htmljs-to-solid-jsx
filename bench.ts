import { transform } from "./transform";
function transformTest() {
  const data = transform(
    `<div>
  hello \${x}!\${x}\${x}
  ${Math.random()}
</>
`,
    "hello.marko"
  );
}

transformTest();
console.time("transform");
console.profile("transform");
for (const i of Array.from({ length: 10000 })) {
  transformTest();
}

console.profileEnd("transform");
console.timeEnd("transform");

import { parse } from "@babel/parser";
import generate from "@babel/generator";

function parseTest() {
  const filename = "hello.js";
  const code = `<div>
  hello {x}!{x}{x}
  ${Math.random()}
</div>`;

  (generate as any as { default: typeof generate }).default(
    parse(code, {
      sourceFilename: "hello.js",
      sourceType: "module",
      strictMode: true,
      plugins: ["jsx"]
    }),
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
}

parseTest();
console.time("parse");
console.profile("parse");
for (const i of Array.from({ length: 10000 })) {
  parseTest();
}

console.profileEnd("parse");
console.timeEnd("parse");
