const dataSource = require("zigbee-herdsman-converters");
const fs = require("node:fs");
const path = require("node:path");
const yaml = require("yaml");

function parseArguments() {
  if (process.argv.length <= 3) {
    console.error("Usage: npm start <output-dir> <package-yaml-path>");
    process.exit(1);
  }

  outputPath = process.argv[2];
  packageYamlPath = process.argv[3];

  if (!fs.existsSync(outputPath) || !fs.lstatSync(outputPath).isDirectory()) {
    console.error("No such directory " + outputPath);
    process.exit(1);
  }

  if (
    !fs.existsSync(packageYamlPath) ||
    !fs.lstatSync(packageYamlPath).isFile()
  ) {
    console.error("No such file " + packageYamlPath);
    process.exit(1);
  }

  return { outputPath, packageYamlPath };
}

function writeDefinitionFiles({ outputPath }) {
  const sanitizeStr = (str) =>
    str
      .replace(/[^a-zA-Z0-9]+/g, "_")
      .replace(/^_+/, "")
      .replace(/_+$/, "");

  for (const definition of dataSource.definitions) {
    const vendor = sanitizeStr(definition.vendor);
    const model = `${sanitizeStr(definition.model)}`;

    const definitionPath = `${outputPath}/definitions/${vendor}/${model}.json`;
    fs.mkdirSync(path.dirname(definitionPath), { recursive: true });
    fs.writeFileSync(definitionPath, JSON.stringify(definition, null, 2));
  }
}

function toConstructorOrTypeName(str) {
  return str
    .replace(/[^a-zA-Z0-9]/g, " ") // Replace non alphanumeric chars with spaces
    .replace(/\s(.)/g, (letter) => letter.toUpperCase()) // Capitalize start of each word
    .replace(/\s/g, "") // Remove spaces
    .replace(/^(.)/, (letter) => letter.toUpperCase()); // Capitalize first letter
}

function propToConstructorName(prop, hsFile) {
  return toConstructorOrTypeName(prop.name);
}

function propToType(prop, hsFile) {
  switch (prop.type) {
    case "binary":
      return "Bool";
    case "enum":
      hsFile.content.push({
        type: "verbatim",
        content: `data ${toConstructorOrTypeName(prop.name)} = ${prop.values
          .map(toConstructorOrTypeName)
          .join(" | ")}`,
      });
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

function getExposedProps(list) {
  const todo = list.constructor === Array ? [...list] : [];
  const props = [];

  const specificTypes = ["light", "switch", "fan", "cover", "lock", "climate"];

  while (todo.length !== 0) {
    const prop = todo.shift();
    if (specificTypes.includes(prop.type)) {
      todo.unshift(...prop.features);
    } else {
      props.push(prop);
    }
  }

  return props;
}

function renderHsFile({ pragmas, moduleName, imports, content }) {
  const buildGadt = (gadt) =>
    [
      `data ${gadt.name} a where`,
      ...gadt.constructors.map((c) => `  ${c.name} :: ${gadt.name} ${c.type}`),
    ].join("\n");
  const buildContent = (c) => {
    switch (c.type) {
      case "gadt":
        return buildGadt(c);
      case "verbatim":
        return c.content;
      default:
        console.error("Unknown content type:", c.type);
        process.exit(1);
    }
  };
  return [
    ...pragmas,
    "",
    `module ${moduleName} where`,
    "",
    ...imports,
    "",
    content.map(buildContent).join("\n\n"),
  ].join("\n");
}

function writeDeviceFiles({ outputPath }) {
  const sanitizeStr = (str) =>
    str
      .replace(/[^a-zA-Z0-9]+/g, "_")
      .replace(/^_+/, "")
      .replace(/_+$/, "");
  const exposedModules = [];

  for (const definition of dataSource.definitions) {
    const vendor = sanitizeStr(definition.vendor);
    const model = sanitizeStr(definition.model);

    const definitionPath = `${outputPath}/src/Zigbee2MQTT/Devices/${vendor}_${model}.hs`;
    const module = `Zigbee2MQTT.Devices.${vendor}_${model}`;
    exposedModules.splice(0, 0, module);

    props = getExposedProps(definition.exposes);

    hsFile = {
      pragmas: [`{-# LANGUAGE GADTs #-}`],
      moduleName: module,
      imports: [],
      content: [],
    };

    hsFile.content.push({
      type: "gadt",
      name: "Prop",
      constructors: props.map((prop) => ({
        name: propToConstructorName(prop, hsFile),
        type: propToType(prop, hsFile),
      })),
    });

    fs.mkdirSync(path.dirname(definitionPath), { recursive: true });
    fs.writeFileSync(definitionPath, renderHsFile(hsFile));
  }

  return exposedModules;
}

function writePackageYaml({ outputPath, packageYamlPath }, exposedModules) {
  const yamlStr = fs.readFileSync(packageYamlPath, { encoding: "utf-8" });
  const packageYaml = yaml.parse(yamlStr);
  packageYaml.library["exposed-modules"].splice(0, 0, ...exposedModules);
  fs.writeFileSync(`${outputPath}/package.yaml`, yaml.stringify(packageYaml));
}

function main() {
  config = parseArguments();

  writeDefinitionFiles(config);
  const exposedModules = writeDeviceFiles(config);
  writePackageYaml(config, exposedModules);
}

main();
