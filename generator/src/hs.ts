

type Constructor = { name: string, fields?: string[] };
type Datatype = { type: "datatype", name: string, constructors: Constructor[] }
;
type GadtConstructor = { name: string, type: string };
type Gadt = { type: "gadt", name: string, constructors: GadtConstructor[] };

type Verbatim = { type: "verbatim", content: string }

type HsContent = Datatype | Gadt | Verbatim
type HsFile = {
  pragmas: string[]
  moduleName: string,
  imports: string[],
  content: HsContent[],
};


function toConstructorOrTypeName(str: string): string {
  return str
    .toString()
    .replace(/([A-Z][a-z])/g, (str) => " " + str)
    .toLowerCase()
    .replace(/[^a-zA-Z0-9]/g, " ") // Replace non alphanumeric chars with spaces
    .replace(/\s([a-z])/g, (letter) => letter.toUpperCase()) // Capitalize start of each word
    .replace(/^([a-z])/, (letter) => letter.toUpperCase()) // Capitalize first letter
    .replace(/\s/g, ""); // Remove spaces
}


function formatConstructor({ name, fields }: Constructor): string {
  return [toConstructorOrTypeName(name), ...fields || []].join(" ");
}


function formatDatatype({ name, constructors }: Datatype): string {
  const constructorStrings = uniq(constructors.map(formatConstructor));
  const oneConstructorOneField = constructorStrings.length === 1 && constructors.every(c => c.fields?.length === 1);
  const type = oneConstructorOneField ? "newtype" : "data";
  return `${type} ${name} = ${constructorStrings.join(" | ")}`
}


function formatGadt({ name, constructors }: Gadt): string {
  return [
    `data ${name} a where`,
    ...constructors.map((c) => `  ${c.name} :: ${name} ${c.type}`),
  ].join("\n");
}

function formatHsContent(item: HsContent): string {
  switch (item.type) {
    case "datatype": return formatDatatype(item);
    case "gadt": return formatGadt(item);
    case "verbatim": return item.content;
  }
}


function renderHsFile({ pragmas, moduleName, imports, content }: HsFile): string {
  return [
    ...uniq(pragmas),
    "",
    `module ${moduleName} where`,
    "",
    ...uniq(imports),
    "",
    ...uniq(content.map(formatHsContent))
  ].join("\n");
}


function uniq<Type>(array: Type[]): Type[] {
  return [...new Set(array)];
}
