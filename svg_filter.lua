-- svg_filter.lua
-- A simple Lua filter to modify SVG files

-- Function to read the entire file content
function read_file(path)
  local file = io.open(path, "r")
  if not file then return nil end
  local content = file:read("*all")
  file:close()
  return content
end

-- Function to write content to a file
function write_file(path, content)
  local file = io.open(path, "w")
  if not file then return false end
  file:write(content)
  file:close()
  return true
end

-- Main filter function
function Filter(doc)
  -- Get the input file from command line arguments
  local input_file = nil
  for i = 1, #arg do
    if arg[i]:match("%.svg$") then
      input_file = arg[i]
      break
    end
  end
  
  if not input_file then return doc end
  
  -- Read the SVG content
  local svg_content = read_file(input_file)
  if not svg_content then return doc end
  
  -- Apply transformations to the SVG content
  -- Here we're just adding a simple metadata comment as an example
  local modified_content = svg_content:gsub(
    "<svg", 
    "<!-- Processed by Lua SVG filter -->\n<svg"
  )
  
  -- You could implement more complex transformations here:
  -- - Change colors
  -- - Add filters
  -- - Modify attributes
  -- - etc.
  
  -- Write the modified content back
  write_file(input_file, modified_content)
  
  return doc
end
