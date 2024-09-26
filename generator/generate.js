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
    fs.writeFileSync(definitionPath, JSON.stringify(definition, null, 4));
  }
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
    const fileContent = [`module ${module} where`].join("\n");
    fs.mkdirSync(path.dirname(definitionPath), { recursive: true });
    fs.writeFileSync(definitionPath, fileContent);
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
