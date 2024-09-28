

type GadtConstructor = { name: string, type: string };
type Gadt = { type: "gadt", name: string, constructors: GadtConstructor[] };
type Verbatim = { type: "verbatim", content: string }
type HsContent = Gadt | Verbatim
type HsFile = {
  pragmas: string[]
  moduleName: string,
  imports: string[],
  content: HsContent[],
};


function toConstructorOrTypeName(str: string): string {
  return str
    .toString()
    .replace(/[^a-zA-Z0-9]/g, " ") // Replace non alphanumeric chars with spaces
    .replace(/\s(.)/g, (letter) => letter.toUpperCase()) // Capitalize start of each word
    .replace(/\s/g, "") // Remove spaces
    .replace(/^(.)/, (letter) => letter.toUpperCase()); // Capitalize first letter
}


function renderHsFile(hsFile: HsFile): string {
  const buildGadt = (gadt: Gadt) =>
    [
      `data ${gadt.name} a where`,
      ...gadt.constructors.map((c) => `  ${c.name} :: ${gadt.name} ${c.type}`),
    ].join("\n");
  const buildContent = (c: HsContent) => {
    switch (c.type) {
      case "gadt": return buildGadt(c);
      case "verbatim": return c.content;
    }
  };
  return [
    ...hsFile.pragmas,
    "",
    `module ${hsFile.moduleName} where`,
    "",
    ...hsFile.imports,
    "",
    hsFile.content.map(buildContent).join("\n\n"),
  ].join("\n");
}
