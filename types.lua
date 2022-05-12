local types = {
    { 0, true, 64, false },
    { 0, true, 32, false },
    { -63, true, 63, false },
    { -31, true, 31, false },
    { -53, false, 53, true },
    { -24, false, 24, true }
}

local function typeToString(t)
    if t[1] == 0 then
        return "u" .. t[3]
    elseif -t[1] == t[3] then
        if t[4] then
            if t[3] == 53 then
                return "f64"
            elseif t[3] == 24 then
                return "f32"
            end
        else
            return "i" .. (t[3] + 1)
        end
    else
        return t[1] .. "..=" .. t[3]
    end
end

local function find(n)
    local res = {}
    for _, v in pairs(types) do
        if (v[2] and (v[1] < n) or (v[1] <= n)) and v[3] > n then
            table.insert(res, v)
        end
    end
    return res
end

local function coerce(s, t)
    return ((s[2] or not t[2]) and s[1] >= t[1] or s[1] > t[1]) and s[3] <= t[3] and (not s[4] or t[4]) -- sorry
end

local function mostCoercible(f)
    local function inner(s)
        for _, t in pairs(f) do
            if not coerce(s, t) then
                return false
            end
        end
        return true
    end

    for _, s in pairs(f) do
        if inner(s) then
            return s
        end
    end
end

local function GCD(f)
    local min, minInclusive, max = f[1][1], f[1][2], f[1][3]
    for i = 2, #f do
        local v = f[i]
        if v[1] > min or (v[1] == min and not v[2] and minInclusive) then
            min = v[1]
            minInclusive = v[2]
        end
        if v[3] < max then
            max = v[3]
        end
    end
    return { min, minInclusive, max, false }
end

print("core types")
for _, v in pairs(types) do
    print(typeToString(v))
end

print()
print("supplementary types")
for i = -63, 63 do
    local f = find(i)
    local lc = mostCoercible(f)
    if lc == nil then
        local newType = GCD(f)
        print(typeToString(newType))
        table.insert(types, newType)
        f = find(i)
        if mostCoercible(f) == nil then
            print()
            print("number 2^" .. i)
            for _, v in pairs(f) do
                print(typeToString(v))
            end
            error("unable to create supplement")
        end
    end
end
