## NAME

confer – The dotfiles manager

## SYNOPSIS

<dl>
    <dt>confer deploy <i>[OPTION]</i></dt>
    <dd style="margin-left: 1rem;">Deploy your configuration files as symbolic links in their configured destination</dd>
    <dt>confer check <i>[OPTION]</i></dt>
    <dd style="margin-left: 1rem;">Ensure that the configured link destinations do not exist.</dd>
</dl>

## DESCRIPTION

*confer* is a dotfiles manager that handles the deployment and synchronisation of your configuration files. 

It is configured in Lua. The [Lua API](#lua-api) enables you to decide where to link your files depending on criteria, like your operating system.

## OPTIONS

### Runtime Options

#### `--dry-run`
Simulate the actions to be executed but doesn't do them.

#### `--verbose`
Make the execution more verbose about what it does.

#### `--version`
Display the version of the tool.

#### `--help`
Display the help message.

### Configuration overrides

#### `--arch=<arch>`
Override the detected architecturein the configuration file.

With `arch` as:

* aarch64
* x86_64

#### `--os=<os>`
Override the operating system detected in the configuration file.

With `os` as:
* darwin
* freebsd
* linux
* windows

#### `--hostname=<hostname>`
Override the host name detected in the configuration file.

### CONFIGURATION

The configuration is written in a Lua file with facts and deployement rules.

To express a symbolic link of your `.gitconfig` file within your home directory, write:

```lua
local git_deployment = confer.fact({
  -- The name of this fact.
  name = "git",
  -- The file or directory that you want to link.
  source = ".gitconfig",
  -- The directory in which the link will be made.
  destination = "~/"
})
```
Then we add a rule that holds potential conditions for this deployment to occur:
The name of the host has to be `my-laptop`. If this condition is not met,
the deployment will be ignored.

```lua
local laptop = confer.deploy({
  hostname = "my-laptop",
  facts = {
    git_deployment,
    },
})

```

### EXAMPLES

#### Example configuration

```lua
local git_deployment = confer.fact({
  name = "git",
  source = "./git",
  destination = "~/"
})

local zsh_deployment = confer.fact({
  name = "zsh",
  sources = {".zsh/", ".zshrc"},
  destination = "~/"
})

local kitty_deployment = confer.fact({
  name = "kitty",
  source = "./kitty",
  destination = "~/.config/"
})

local laptop = confer.deploy({
  hostname = "my-laptop",
  facts = {
    git_deployment,
    zsh_deployment,
    kitty_deployment,
    },
})

local server = confer.deploy({
  hostname = "my-server",
  facts = {
    git_deployment,
    zsh_deployment,
    },
})

return {
  laptop,
  server;
}
```

## Lua API

Confer provides its own utilities to be used in Lua:

### Host API

#### `host.os`:
The operating system identifier of the host.
It is either inferred from your host, or overriden by the `--os` command-line option.

Possible values:
* darwin
* freebsd
* linux
* windows
 
#### `host.arch`:
The architecture identifier of the host.
It is either inferred from your host, or overriden by the `--arch` command-line option.

Possible values:
* aarch64
* x86_64

#### `host.hostname`
The hostname of the host.
It is either inferred from your host, or overriden by the `--hostname` command-line option.

### User API

#### `user.home`
The home directory of the current user.

### Confer API

#### `confer.deploy`
A dictionary called `Deployment` that takes the following constructors:
* name: String, name of this deployment
* source: FilePath, directory or file which is going to be linked to the destination
* sources: Array FilePath, several sources that will be linked to the same destination directory
* destination: FilePath, directory to which the `source` will be linked

#### `confer.deployments`
* deployments: Array Deployment, all the deployments you want to run
* hostname: String, the hostname of the machine in which the deployment will occur. If not specified, will occur on all hosts.
* architecture: String, the architecture of the machine in which the deployment will occur. If not specified, will occur on all architectures.
* os: String, the operating system of the machine in which the deployment will occur. If not specified, will occur on all operating systems.
