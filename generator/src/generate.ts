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

type BinaryProp = { type: "binary", name: string, property: string, value_on: string | boolean, value_off: string | boolean, value_toggle?: string };
type CompositeProp = { type: "composite", name: string, property: string, features: GenericProp[] };
type EnumProp = { type: "enum", name: string, property: string, values: string[] };
type ListProp = { type: "list", name: string, property: string, item_type: GenericProp}
type NumericProp = { type: "numeric", name: string, property: string };
type TextProp = { type: "text", name: string, property: string };

type GenericProp = BinaryProp | CompositeProp | EnumProp | ListProp | NumericProp | TextProp

type SpecificProp = {
  type: "light" | "switch" | "fan" | "cover" | "lock" | "climate",
  features: GenericProp[]
}

type Prop = GenericProp | SpecificProp

function propToConstructorName(prop: GenericProp) {
  return toConstructorOrTypeName(prop.property);
}

function propsToType(hsFile: HsFile, prefix: string, ...props: [GenericProp, ...GenericProp[]]): string {
  const typeName = toConstructorOrTypeName(prefix + " " + (props[0].property || props[0].name));

  const constructors: Constructor[] = [];
  props.forEach(prop => {
    switch (prop.type) {
      case "binary":
        const isTrueAndFalse =
          (prop.value_off === false && prop.value_on === true) ||
          (prop.value_off === true && prop.value_on === false);
        if (isTrueAndFalse && prop.value_toggle === undefined) {
          constructors.push({ name: prop.name, fields: [ "Bool" ]});
        } else {
          const values = [prop.value_on, prop.value_off, ...(prop.value_toggle ? [prop.value_toggle] : [])];
          values.forEach(val => constructors.push({ name: val.toString(), fields: [ ]}));
        }
        break;
      case "composite":
        const fields = prop.features.map((p) => propsToType(hsFile, prefix + " " + (prop.property || prop.name), p));
        constructors.push({ name: prop.name, fields});
        break;
      case "enum":
        prop.values.forEach((str) => constructors.push({ name: str, fields: [] }));
        break;
      case "list":
        constructors.push({ name: prop.name, fields: [ `[${propsToType(hsFile, prefix + " " + (prop.property || prop.name), prop.item_type)}]` ]});
        break;
      case "numeric":
        constructors.push({ name: prop.name, fields: [ "Int" ]});
        break;
      case "text":
        hsFile.imports.push("import qualified Data.Text as T");
        constructors.push({ name: prop.name, fields: [ "T.Text" ]});
        break;
    }
  });

  if (constructors.length === 1 && constructors[0]?.fields?.length === 1 && constructors[0]?.fields[0] !== undefined) {
    return constructors[0]?.fields[0];
  }

  hsFile.content.push({
    type: "datatype",
    name: typeName,
    constructors: constructors.map((c) => ({ ...c, name: prefix + " " + (props[0].property || props[0].name) + " " + c.name }))
  });

  return typeName;
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
      .replace(/^\s*(.)/, (letter) => letter.toUpperCase()) // Capitalize first letter
      .replace(/^_+/, "")
      .replace(/_+$/, "");
  const exposedModules: string[] = [];

  for (const definition of dataSource.definitions) {
    const vendor = sanitizeStr(definition.vendor);
    const model = sanitizeStr(definition.model);

    const definitionPath = `${config.outputPath}/src/Zigbee2MQTT/Devices/${vendor}_${model}.hs`;
    const module = `Zigbee2MQTT.Devices.${vendor}_${model}`;
    exposedModules.splice(0, 0, module);

    const hsFile: HsFile = {
      pragmas: [`{-# LANGUAGE GADTs #-}`],
      moduleName: module,
      imports: [],
      content: [],
    };

    const groupedProps: Partial<Record<string, GenericProp[]>> = Object.groupBy(
      getExposedProps(definition.exposes),
      (prop: GenericProp) => prop.property
    );

    function isNotEmpty<Type>(a: [string, Type[] | undefined]): a is [string, [Type, ...Type[]]] {
      return a[1] !== undefined && a[1].length !== 0;
    }

    const constructors = Object
      .entries(groupedProps)
      .filter(isNotEmpty)
      .map(([key, props]) => ({
        name: toConstructorOrTypeName("Prop " + key),
        type: propsToType(hsFile, "", ...props)

      }))

    hsFile.content.push({
      type: "gadt",
      name: toConstructorOrTypeName("Prop"),
      constructors
    });

    fs.mkdirSync(path.dirname(definitionPath), { recursive: true });
    fs.writeFileSync(definitionPath, renderHsFile(hsFile));
  }

  return exposedModules;
}

function writePackageYaml(config: Config, exposedModules: string[]): void {
  const yamlStr = fs.readFileSync(config.packageYamlPath, { encoding: "utf-8" });
  const packageYaml = yaml.parse(yamlStr);
  exposedModules = [...new Set(exposedModules)]
  packageYaml.library["exposed-modules"].splice(0, 0, ...exposedModules);
  fs.writeFileSync(`${config.outputPath}/package.yaml`, yaml.stringify(packageYaml));
}

function main(): void {
  const config: Config = parseArguments();

  const exposedModules: string[] = writeDeviceFiles(config);
  writePackageYaml(config, exposedModules);
}

main();
