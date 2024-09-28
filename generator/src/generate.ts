const dataSource = require("zigbee-herdsman-converters");
const fs = require("node:fs");
const path = require("node:path");
const yaml = require("yaml");

type Config = { outputPath: string, packageYamlPath: string }

function parseArguments(): Config {
  if (process.argv.length <= 3) {
    console.error("Usage: npm start <output-dir> <package-yaml-path>");
    process.exit(1);
  }

  const outputPath: string | undefined = process.argv[2];
  const packageYamlPath: string | undefined = process.argv[3];

  if (outputPath === undefined || !fs.existsSync(outputPath) || !fs.lstatSync(outputPath).isDirectory()) {
    console.error("No such directory " + outputPath);
    process.exit(1);
  }

  if (
    packageYamlPath === undefined ||
    !fs.existsSync(packageYamlPath) ||
    !fs.lstatSync(packageYamlPath).isFile()
  ) {
    console.error("No such file " + packageYamlPath);
    process.exit(1);
  }

  return { outputPath, packageYamlPath };
}

type BinaryProp = { type: "binary", name: string, value_on: string | boolean, value_off: string | boolean, value_toggle?: string };
type EnumProp = { type: "enum", name: string, values: string[] };
type NumericProp = { type: "numeric", name: string };
type TextProp = { type: "text", name: string };
type CompositeProp = { type: "composite", name: string };

type GenericProp = BinaryProp | CompositeProp | EnumProp | NumericProp | TextProp

type SpecificProp = {
  type: "light" | "switch" | "fan" | "cover" | "lock" | "climate",
  features: GenericProp[]
}

type Prop = GenericProp | SpecificProp

function propToConstructorName(prop: GenericProp) {
  return toConstructorOrTypeName(prop.name);
}

function createEnum(type: string, hsFile: HsFile, ...constructors: string[]) {
  hsFile.content.push({
    type: "verbatim",
    content: `data ${toConstructorOrTypeName(type)} = ${constructors.map((str) =>
      `${toConstructorOrTypeName(type)}${toConstructorOrTypeName(str)}`
    ).join(" | ")}`
  });
}

function binaryPropToType(prop: BinaryProp, hsFile: HsFile): string {
  if (prop.value_on === true && prop.value_off === false && prop.value_toggle === undefined) {
    return "Bool";
  } else {
    const props = [prop.value_on.toString(), prop.value_off.toString(), ...(prop.value_toggle ? [prop.value_toggle] : [])];
    createEnum(prop.name, hsFile, ...props);
    return toConstructorOrTypeName(prop.name);
  }
}

function propToType(prop: GenericProp, hsFile: HsFile): string {
  switch (prop.type) {
    case "binary":
      return binaryPropToType(prop, hsFile);
    case "enum":
      createEnum(prop.name, hsFile, ...prop.values);
      return toConstructorOrTypeName(prop.name);
    case "numeric":
      return "Int";
    case "text":
      hsFile.imports.push("import qualified Data.Text as T");
      return "T.Text";
    case "composite":
      return toConstructorOrTypeName(prop.name);
  }
}

function getExposedProps(list: Prop[]): GenericProp[] {
  const todo = list.constructor === Array ? [...list] : [];
  const props = [];
  let prop;

  while (prop = todo.shift()) {
    switch (prop.type) {
      case "light":
      case "switch":
      case "fan":
      case "cover":
      case "lock":
      case "climate":
        todo.unshift(...prop.features);
        break;
      default:
        props.push(prop);
    }
  }

  return props;
}

function writeDeviceFiles(config: Config): string[] {
  const sanitizeStr = (str: string) =>
    str
      .replace(/[^a-zA-Z0-9]+/g, "_")
      .replace(/^_+/, "")
      .replace(/_+$/, "");
  const exposedModules: string[] = [];

  for (const definition of dataSource.definitions) {
    const vendor = sanitizeStr(definition.vendor);
    const model = sanitizeStr(definition.model);

    const definitionPath = `${config.outputPath}/src/Zigbee2MQTT/Devices/${vendor}_${model}.hs`;
    const module = `Zigbee2MQTT.Devices.${vendor}_${model}`;
    exposedModules.splice(0, 0, module);

    const props: GenericProp[] = getExposedProps(definition.exposes);

    const hsFile: HsFile = {
      pragmas: [`{-# LANGUAGE GADTs #-}`],
      moduleName: module,
      imports: [],
      content: [],
    };

    hsFile.content.push({
      type: "gadt",
      name: "Prop",
      constructors: props.map((prop) => ({
        name: propToConstructorName(prop),
        type: propToType(prop, hsFile),
      })),
    });

    fs.mkdirSync(path.dirname(definitionPath), { recursive: true });
    fs.writeFileSync(definitionPath, renderHsFile(hsFile));
  }

  return exposedModules;
}

function writePackageYaml(config: Config, exposedModules: string[]): void {
  const yamlStr = fs.readFileSync(config.packageYamlPath, { encoding: "utf-8" });
  const packageYaml = yaml.parse(yamlStr);
  packageYaml.library["exposed-modules"].splice(0, 0, ...exposedModules);
  fs.writeFileSync(`${config.outputPath}/package.yaml`, yaml.stringify(packageYaml));
}

function main(): void {
  const config: Config = parseArguments();

  const exposedModules: string[] = writeDeviceFiles(config);
  writePackageYaml(config, exposedModules);
}

main();
