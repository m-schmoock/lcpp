title = "lcpp"
project = "API Doc"..[[

<!-- your html here -->

<script>
var projectname = "lcpp"
var projectsub = "a Lua C-PreProcessor"

document.getElementById("product_logo").innerHTML = '<img src="/lua-logo-'+projectname+'.png" alt="logo"/>'
document.getElementById("product_name").innerHTML = '<big>'+projectname+'</big>'
document.getElementById("product_description").innerHTML = projectsub
</script>
<style>
#content { width:initial ; }
</style>

]]

description = [[
	-- single module has no description
]]

readme = {
	"README.md",
}

file = {
	".",
}
dir = "luadoc"
ext = "html"
format = "discount"

