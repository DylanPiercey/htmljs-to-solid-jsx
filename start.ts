import path from "path";
import fs from "fs/promises";
import { fileURLToPath } from "url";
import { transform } from "./transform";
import { format } from "prettier";
import { codeFrameColumns } from "@babel/code-frame";

const demoDir = path.join(fileURLToPath(import.meta.url), "..", "demo");
const inputFile = path.join(demoDir, "input.marko");
const outputFile = path.join(demoDir, "output.jsx");

await compile();

for await (const _ of fs.watch(inputFile)) {
  await compile();
}

async function compile() {
  const input = await fs.readFile(inputFile, "utf8");

  try {
    const output = transform(input, inputFile);
    const formatted = format(output.code, { parser: "babel" });
    await fs.writeFile(outputFile, formatted);
  } catch (err) {
    if (err.loc) {
      err.message += `\n${codeFrameColumns(
        input,
        err.loc.start ? err.loc : { start: err.loc },
        {
          highlightCode: true,
        }
      )}`;
    }
    console.error(err);
  }
}
