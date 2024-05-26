local user = require("user")

local git_deployment = confer.fact({
  name = "git",
  source = ".gitconfig",
  destination = user.home
})

local zsh_deployment = confer.fact({
  name = "zsh",
  source = ".zsh",
  destination = user.home
})

local kitty_deployment = confer.fact({
  name = "kitty",
  source = "kitty",
  destination = user.home .. "/.config"
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
