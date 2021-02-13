module AstConfiguration

let getSchemaUrl (provider : string) version =
    "https://raw.githubusercontent.com/pulumi/pulumi-"+
    provider.ToLower() +
    "/v" +
    version +
    "/provider/cmd/pulumi-resource-" +
    provider.ToLower() +
    "/schema.json"