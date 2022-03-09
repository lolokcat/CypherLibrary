local Library = {}
local json = { _version = "0.1.2" }

-------------------------------------------------------------------------------
-- Encode
-------------------------------------------------------------------------------

local encode

local escape_char_map = {
  [ "\\" ] = "\\",
  [ "\"" ] = "\"",
  [ "\b" ] = "b",
  [ "\f" ] = "f",
  [ "\n" ] = "n",
  [ "\r" ] = "r",
  [ "\t" ] = "t",
}

local escape_char_map_inv = { [ "/" ] = "/" }
for k, v in pairs(escape_char_map) do
  escape_char_map_inv[v] = k
end


local function escape_char(c)
  return "\\" .. (escape_char_map[c] or string.format("u%04x", c:byte()))
end


local function encode_nil(val)
  return "null"
end


local function encode_table(val, stack)
  local res = {}
  stack = stack or {}

  -- Circular reference?
  if stack[val] then error("circular reference") end

  stack[val] = true

  if rawget(val, 1) ~= nil or next(val) == nil then
    -- Treat as array -- check keys are valid and it is not sparse
    local n = 0
    for k in pairs(val) do
      if type(k) ~= "number" then
        error("invalid table: mixed or invalid key types")
      end
      n = n + 1
    end
    if n ~= #val then
      error("invalid table: sparse array")
    end
    -- Encode
    for i, v in ipairs(val) do
      table.insert(res, encode(v, stack))
    end
    stack[val] = nil
    return "[" .. table.concat(res, ",") .. "]"

  else
    -- Treat as an object
    for k, v in pairs(val) do
      if type(k) ~= "string" then
        error("invalid table: mixed or invalid key types")
      end
      table.insert(res, encode(k, stack) .. ":" .. encode(v, stack))
    end
    stack[val] = nil
    return "{" .. table.concat(res, ",") .. "}"
  end
end


local function encode_string(val)
  return '"' .. val:gsub('[%z\1-\31\\"]', escape_char) .. '"'
end


local function encode_number(val)
  -- Check for NaN, -inf and inf
  if val ~= val or val <= -math.huge or val >= math.huge then
    error("unexpected number value '" .. tostring(val) .. "'")
  end
  return string.format("%.14g", val)
end


local type_func_map = {
  [ "nil"     ] = encode_nil,
  [ "table"   ] = encode_table,
  [ "string"  ] = encode_string,
  [ "number"  ] = encode_number,
  [ "boolean" ] = tostring,
}


encode = function(val, stack)
  local t = type(val)
  local f = type_func_map[t]
  if f then
    return f(val, stack)
  end
  error("unexpected type '" .. t .. "'")
end


function json.encode(val)
  return ( encode(val) )
end


-------------------------------------------------------------------------------
-- Decode
-------------------------------------------------------------------------------

local parse

local function create_set(...)
  local res = {}
  for i = 1, select("#", ...) do
    res[ select(i, ...) ] = true
  end
  return res
end

local space_chars   = create_set(" ", "\t", "\r", "\n")
local delim_chars   = create_set(" ", "\t", "\r", "\n", "]", "}", ",")
local escape_chars  = create_set("\\", "/", '"', "b", "f", "n", "r", "t", "u")
local literals      = create_set("true", "false", "null")

local literal_map = {
  [ "true"  ] = true,
  [ "false" ] = false,
  [ "null"  ] = nil,
}


local function next_char(str, idx, set, negate)
  for i = idx, #str do
    if set[str:sub(i, i)] ~= negate then
      return i
    end
  end
  return #str + 1
end


local function decode_error(str, idx, msg)
  local line_count = 1
  local col_count = 1
  for i = 1, idx - 1 do
    col_count = col_count + 1
    if str:sub(i, i) == "\n" then
      line_count = line_count + 1
      col_count = 1
    end
  end
  error( string.format("%s at line %d col %d", msg, line_count, col_count) )
end


local function codepoint_to_utf8(n)
  -- http://scripts.sil.org/cms/scripts/page.php?site_id=nrsi&id=iws-appendixa
  local f = math.floor
  if n <= 0x7f then
    return string.char(n)
  elseif n <= 0x7ff then
    return string.char(f(n / 64) + 192, n % 64 + 128)
  elseif n <= 0xffff then
    return string.char(f(n / 4096) + 224, f(n % 4096 / 64) + 128, n % 64 + 128)
  elseif n <= 0x10ffff then
    return string.char(f(n / 262144) + 240, f(n % 262144 / 4096) + 128,
                       f(n % 4096 / 64) + 128, n % 64 + 128)
  end
  error( string.format("invalid unicode codepoint '%x'", n) )
end


local function parse_unicode_escape(s)
  local n1 = tonumber( s:sub(1, 4),  16 )
  local n2 = tonumber( s:sub(7, 10), 16 )
   -- Surrogate pair?
  if n2 then
    return codepoint_to_utf8((n1 - 0xd800) * 0x400 + (n2 - 0xdc00) + 0x10000)
  else
    return codepoint_to_utf8(n1)
  end
end


local function parse_string(str, i)
  local res = ""
  local j = i + 1
  local k = j

  while j <= #str do
    local x = str:byte(j)

    if x < 32 then
      decode_error(str, j, "control character in string")

    elseif x == 92 then -- `\`: Escape
      res = res .. str:sub(k, j - 1)
      j = j + 1
      local c = str:sub(j, j)
      if c == "u" then
        local hex = str:match("^[dD][89aAbB]%x%x\\u%x%x%x%x", j + 1)
                 or str:match("^%x%x%x%x", j + 1)
                 or decode_error(str, j - 1, "invalid unicode escape in string")
        res = res .. parse_unicode_escape(hex)
        j = j + #hex
      else
        if not escape_chars[c] then
          decode_error(str, j - 1, "invalid escape char '" .. c .. "' in string")
        end
        res = res .. escape_char_map_inv[c]
      end
      k = j + 1

    elseif x == 34 then -- `"`: End of string
      res = res .. str:sub(k, j - 1)
      return res, j + 1
    end

    j = j + 1
  end

  decode_error(str, i, "expected closing quote for string")
end


local function parse_number(str, i)
  local x = next_char(str, i, delim_chars)
  local s = str:sub(i, x - 1)
  local n = tonumber(s)
  if not n then
    decode_error(str, i, "invalid number '" .. s .. "'")
  end
  return n, x
end


local function parse_literal(str, i)
  local x = next_char(str, i, delim_chars)
  local word = str:sub(i, x - 1)
  if not literals[word] then
    decode_error(str, i, "invalid literal '" .. word .. "'")
  end
  return literal_map[word], x
end


local function parse_array(str, i)
  local res = {}
  local n = 1
  i = i + 1
  while 1 do
    local x
    i = next_char(str, i, space_chars, true)
    -- Empty / end of array?
    if str:sub(i, i) == "]" then
      i = i + 1
      break
    end
    -- Read token
    x, i = parse(str, i)
    res[n] = x
    n = n + 1
    -- Next token
    i = next_char(str, i, space_chars, true)
    local chr = str:sub(i, i)
    i = i + 1
    if chr == "]" then break end
    if chr ~= "," then decode_error(str, i, "expected ']' or ','") end
  end
  return res, i
end


local function parse_object(str, i)
  local res = {}
  i = i + 1
  while 1 do
    local key, val
    i = next_char(str, i, space_chars, true)
    -- Empty / end of object?
    if str:sub(i, i) == "}" then
      i = i + 1
      break
    end
    -- Read key
    if str:sub(i, i) ~= '"' then
      decode_error(str, i, "expected string for key")
    end
    key, i = parse(str, i)
    -- Read ':' delimiter
    i = next_char(str, i, space_chars, true)
    if str:sub(i, i) ~= ":" then
      decode_error(str, i, "expected ':' after key")
    end
    i = next_char(str, i + 1, space_chars, true)
    -- Read value
    val, i = parse(str, i)
    -- Set
    res[key] = val
    -- Next token
    i = next_char(str, i, space_chars, true)
    local chr = str:sub(i, i)
    i = i + 1
    if chr == "}" then break end
    if chr ~= "," then decode_error(str, i, "expected '}' or ','") end
  end
  return res, i
end


local char_func_map = {
  [ '"' ] = parse_string,
  [ "0" ] = parse_number,
  [ "1" ] = parse_number,
  [ "2" ] = parse_number,
  [ "3" ] = parse_number,
  [ "4" ] = parse_number,
  [ "5" ] = parse_number,
  [ "6" ] = parse_number,
  [ "7" ] = parse_number,
  [ "8" ] = parse_number,
  [ "9" ] = parse_number,
  [ "-" ] = parse_number,
  [ "t" ] = parse_literal,
  [ "f" ] = parse_literal,
  [ "n" ] = parse_literal,
  [ "[" ] = parse_array,
  [ "{" ] = parse_object,
}


parse = function(str, idx)
  local chr = str:sub(idx, idx)
  local f = char_func_map[chr]
  if f then
    return f(str, idx)
  end
  decode_error(str, idx, "unexpected character '" .. chr .. "'")
end


function json.decode(str)
  if type(str) ~= "string" then
    error("expected argument of type string, got " .. type(str))
  end
  local res, idx = parse(str, next_char(str, 1, space_chars, true))
  idx = next_char(str, idx, space_chars, true)
  if idx <= #str then
    decode_error(str, idx, "trailing garbage")
  end
  return res
end

local succ, err = pcall(function()
	if not isfile("cyphersettings.json") then
		writefile("cyphersettings.json",json.encode(Library))
		if not isfile("version.cyph") then
			writefile("version.cyph", "1.00")
		else
			local Version = readfile("version.cyph")
			Library["Info"]["Version"] = Version
		end
	else
		local content = readfile("cyphersettings.json")
		Library = json.decode(content)
	end
end)

if err then 
	print(err)
end

function checkFunc ()
	local function isAlreadyLoaded()
		if game.Players.LocalPlayer.PlayerGui:FindFirstChild("LibraryGui") then 
			return true
		else
			return false
		end
	end
	if isAlreadyLoaded() then 
		game.Players.LocalPlayer.PlayerGui.LibraryGui:Destroy()
	end
end

checkFunc()

function Library:Construct(name)
	local defaultname = "Balls Hub"
	name = name or defaultname

	local lib = {
		loadguiasset = function(Id, Parent)
		    local enc = false
		    local object
			local succ, err = pcall(function()
				local Loaded = game:GetObjects("rbxassetid://"..Id)[1]
				Loaded.Parent = Parent
				object = Loaded
				enc = true
			end)
			if succ and enc ~= false then 
				return object
			else
			    warn(err)
			    return false
			end
		end;
		tweenasset = function(asset, propertiestable, info)
			local Tween = game:GetService("TweenService")
			local T = Tween:Create(asset, info, propertiestable)
			
			T:Play()
		end;
	}
	local Main = lib.loadguiasset(9047859847, game.Players.LocalPlayer.PlayerGui)
	Main.ResetOnSpawn = true
	game.Players.LocalPlayer.PlayerGui.LibraryGui.MainFrame.TextLabel.Text = name.." // "..1.00
	
	local UserInputService = game:GetService("UserInputService")
    local gui = Main.MainFrame
    local dragging
    local dragInput
    local dragStart
    local startPos
    local function update(input)
        local delta = input.Position - dragStart
        gui.Position = UDim2.new(startPos.X.Scale, startPos.X.Offset + delta.X, startPos.Y.Scale, startPos.Y.Offset + delta.Y)
    end
    gui.InputBegan:Connect(function(input)
        if input.UserInputType == Enum.UserInputType.MouseButton1 or input.UserInputType == Enum.UserInputType.Touch then
        	dragging = true
        	dragStart = input.Position
        	startPos = gui.Position
        		
        	input.Changed:Connect(function()
        		if input.UserInputState == Enum.UserInputState.End then
        			dragging = false
        		end
        	end)
        end
    end)
    gui.InputChanged:Connect(function(input)
        if input.UserInputType == Enum.UserInputType.MouseMovement or input.UserInputType == Enum.UserInputType.Touch then
        	dragInput = input
        end
    end)
    UserInputService.InputChanged:Connect(function(input)
        if input == dragInput and dragging then
        	update(input)
        end
    end)
	
	if game.Players.LocalPlayer.PlayerGui:FindFirstChild("LibraryGui") then 
	    print("Continue") -- continue execution
	else 
	    return -- yield
	end
	
	local currentpage
	
	function search()
	    if not currentpage then
	        return 
	    else
	        for index,thing in pairs(currentpage:GetChildren()) do 
	            if thing:IsA("TextButton") then 
	                if string.find(thing.TextLabel.Text, game.Players.LocalPlayer.PlayerGui.LibraryGui.MainFrame.TopBar.TextBox.Text) then
	                    thing.Visible = true 
	                else
	                    thing.Visible = false 
	                end 
	            end 
	        end 
	    end 
	end
	
	spawn(function()
	    while wait() do 
	        if game.Players.LocalPlayer.PlayerGui.LibraryGui.MainFrame.TopBar.TextBox.Text ~= "" or game.Players.LocalPlayer.PlayerGui.LibraryGui.MainFrame.TopBar.TextBox.Text ~= " " then 
	            search()
	        end 
	    end 
    end)
	local TabLib = {}
	function TabLib.new(name)
	        name = name or "Tab1"
	        if name then 
	            local obj = lib.loadguiasset(9047919269, game.Players.LocalPlayer.PlayerGui.LibraryGui.MainFrame.TabFrame)
	            local Page = lib.loadguiasset(9048179760, game.Players.LocalPlayer.PlayerGui.LibraryGui.MainFrame.TabPages)
	            
	            local function UpdateSize() 
                    wait() 
                    local contentsize = Page.UIListLayout.AbsoluteContentSize
                    Page.CanvasSize = UDim2.new(0,contentsize.X,0,contentsize.Y)
                end
                                        
                Page.ChildAdded:Connect(UpdateSize)
	            
	            obj.Text = name
	            obj.AutoButtonColor = false
	            
	            Page.Visible = false
	            
	            obj.MouseButton1Down:Connect(function()
	                for index,b in next,game.Players.LocalPlayer.PlayerGui.LibraryGui.MainFrame.TabPages:GetChildren() do
	                    b.Visible = false
	                end 
                    Page.Visible = true
                    currentpage = Page
                end)
	            if obj ~= false and obj2 ~= false then 
	                local ElementLib = {}
	                function ElementLib.new(type, name, callback, options)
	                        local function default() 
    	                        type = type or "Button"
    	                        name = name or "Thing"
    	                        callback = callback or function() end
    	                        options = options or {}
    	                    end
    	                    local function createButton(bcallback, name)
    	                        local Button = lib.loadguiasset(9047992217, Page)
    	                        Button.AutoButtonColor = false
    	                        if Button ~= false and Button ~= nil then
    	                           Button.MouseButton1Down:Connect(function()
                                        bcallback()
    	                           end)
    	                           Button.TextLabel.Text = name
    	                        end
    	                    end
    	                    local function createTextBox(bcallback, name)
    	                       local Textbox = lib.loadguiasset(9048595094, Page) 
    	                       Textbox.AutoButtonColor = false
    	                       if Textbox ~= false and Textbox ~= nil then 
    	                           Textbox.TextBox.FocusLost:Connect(function()
    	                               bcallback(Textbox.TextBox.Text)
    	                           end)
    	                           Textbox.TextLabel.Text = name 
    	                       end 
    	                    end
    	                    local function createDropDown(bcallback, name, optionz, updatefunc)
    	                        local DropButton = lib.loadguiasset(9048693088, Page) 
    	                        local DropFrame = lib.loadguiasset(9048698851, game.Players.LocalPlayer.PlayerGui.LibraryGui.MainFrame.Container)
    	                        
    	                        DropButton.TextLabel.Text = name
    	                        DropButton.AutoButtonColor = false
    	                        
    	                        local tog = false
    	                        
    	                        DropFrame.Visible = false
    	                        
    	                        DropButton.MouseButton1Down:Connect(function()
                                    tog = not tog 
                                    if tog == true then 
                                        DropFrame.Visible = true
                                        lib.tweenasset(DropButton.code, {Rotation = 0}, TweenInfo.new(0.3, Enum.EasingStyle.Cubic, Enum.EasingDirection.InOut))
                                    else 
                                        DropFrame.Visible = false
                                        lib.tweenasset(DropButton.code, {Rotation = 90}, TweenInfo.new(0.3, Enum.EasingStyle.Cubic, Enum.EasingDirection.InOut))
                                    end
    	                        end)
    	                        
    	                        local function UpdateSize() 
                                    wait() 
                                    local contentsize = DropFrame.DropContainer.UIListLayout.AbsoluteContentSize
                                    DropFrame.DropContainer.CanvasSize = UDim2.new(0,contentsize.X,0,contentsize.Y)
    	                        end
                            
                                DropFrame.DropContainer.ChildAdded:Connect(UpdateSize)
    	                        
    	                        for index,option in pairs(optionz) do 
    	                           print(option)
    	                           local ContainerButton = lib.loadguiasset(9048819229, DropFrame.DropContainer)
    	                                
    	                           ContainerButton.Text = option 
    	                                
    	                           ContainerButton.MouseButton1Down:Connect(function()
    	                                bcallback(option) 
    	                           end)
    	                        end
				spawn(function()
					while wait() do
						if updatefunc then 
							 DropFrame.DropContainer:ClearAllChildren()
							 for index,option in pairs(updatefunc) do 
							   print(option)
							   local ContainerButton = lib.loadguiasset(9048819229, DropFrame.DropContainer)

							   ContainerButton.Text = option 

							   ContainerButton.MouseButton1Down:Connect(function()
								bcallback(option) 
							   end)
							end
						end
					end
				end)
    	                    end
                            local function createToggle(bcallback, name)
                                local Toggle = lib.loadguiasset(9048996144, Page)
                                
                                Toggle.AutoButtonColor = false 
                                Toggle.TextLabel.Text = name
                                
                                local tog = false
                                
                                Toggle.MouseButton1Down:Connect(function()
                                    tog = not tog 
                                    if tog == true then 
                                        lib.tweenasset(Toggle.check_box, {ImageTransparency = 0}, TweenInfo.new(0.3))
                                    else
                                        lib.tweenasset(Toggle.check_box, {ImageTransparency = 1}, TweenInfo.new(0.3))
                                    end
				    bcallback(tog)
                                end)
						
				game.Players.LocalPlayer.Character.Humanoid.Died:Connect(function()
					bcallback(false)
				end)
                            end
    	                    default()
    	                    if type == "Button" then 
    	                        if not callback or not name then
    	                            return
    	                        else
    	                            createButton(callback, name)
    	                        end
	                        elseif type == "TextBox" then 
	                            if not callback or not name then
	                                return 
	                            else 
	                                createTextBox(callback, name)
	                            end
	                        elseif type == "DropDown" then 
	                            if not callback or not name or not options then 
	                                return 
	                            else
	                                createDropDown(callback, name, options)
	                            end
	                        elseif type == "Toggle" then 
	                            if not callback or not name then 
	                                return 
	                            else
	                                createToggle(callback, name)
	                            end
	                       end
	                end
	                return ElementLib
	            end
	        else
	            return
	        end
	end
	return TabLib
end
return Library
