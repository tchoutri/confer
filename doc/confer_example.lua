
local confer = require("confer")
print(confer)

local git_deployment = confer.fact({
  name = "git",
  source = ".gitconfig",
  destination = ""
})

local zsh_deployment = confer.fact({
  name = "zsh",
  source = ".zsh",
  destination = ""
})

local kitty_deployment = confer.fact({
  name = "kitty",
  source = "./kitty",
  destination = ""
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
  server
}
