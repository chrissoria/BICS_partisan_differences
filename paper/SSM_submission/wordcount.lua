-- wordcount.lua
-- Counts words in the document body and appends a word count line.
local words = 0

local function count(el)
  if el.text and el.text:match("%P") then
    words = words + 1
  end
end

local wordcount_walker = {
  Str  = count,
  Code = function(el)
    for _ in el.text:gmatch("%S+") do words = words + 1 end
  end,
}

function Pandoc(el)
  -- walk all blocks
  for _, block in ipairs(el.blocks) do
    pandoc.walk_block(block, wordcount_walker)
  end
  -- append word count paragraph at the end
  local note = pandoc.Para({
    pandoc.Strong({pandoc.Str("Word count: " .. words)})
  })
  table.insert(el.blocks, note)
  return el
end
