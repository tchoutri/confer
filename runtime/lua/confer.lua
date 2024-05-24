local confer = {}

--- @class (exact) confer.Fact
--- @field name string 
--- @field source string
--- @field destination string 
confer.Fact = {}

--- @param fact confer.Fact
--- @return confer.Fact
function confer.fact(fact)
  return fact
end

--- @class (exact) confer.Deployment
--- @field hostname string?
--- @field arch string?
--- @field os string?
--- @field facts confer.Fact[]
confer.Deployment = {}

--- @param deployment confer.Deployment
--- @return confer.Deployment
function confer.deploy(deployment)
  return deployment
end

return confer
