-- Imports {{{
pcall(require, "luarocks.loader")

-- Standard awesome library
local gears = require("gears")
local awful = require("awful")
local gmath = require("gears.math")
require("awful.autofocus")

-- Widget and layout library
local wibox = require("wibox")

-- Theme handling library
--
local beautiful = require("beautiful")

-- Notification library
local naughty = require("naughty")
local menubar = require("menubar")
local hotkeys_popup = require("awful.hotkeys_popup")
local ezconfig = require('ezconfig')
package.loaded["naughty.dbus"] = {}

local capi =
{
    mouse = mouse,
    screen = screen,
    client = client,
    awesome = awesome,
    root = root,
}
-- END Imports }}}

-- {{{ Error handling
-- Check if awesome encountered an error during startup and fell back to
-- another config (This code will only ever execute for the fallback config)
if awesome.startup_errors then
    naughty.notify({ preset = naughty.config.presets.critical,
                     title = "Oops, there were errors during startup!",
                     text = awesome.startup_errors })
end

-- Handle runtime errors after startup
do
    local in_error = false
    awesome.connect_signal("debug::error", function (err)
        -- Make sure we don't go into an endless error loop
        if in_error then return end
        in_error = true

        naughty.notify({ preset = naughty.config.presets.critical,
                         title = "Oops, an error happened!",
                         text = tostring(err) })
        in_error = false
    end)
end
-- }}}

-- DEFAULTS {{{
beautiful.init(gears.filesystem.get_configuration_dir() .. "themes/zenburn/theme.lua")

-- local nice = require("nice")
-- nice()

beautiful.gap_single_client = true

terminal = "alacritty"
editor = os.getenv("EDITOR") or "nvim"
editor_cmd = terminal .. " -e " .. editor
modkey = "Mod4"
-- meh = "Control, Shift, Mod1"
meh = { "Control", "Shift", "Mod1" }

-- Table of layouts to cover with awful.layout.inc, order matters.
awful.layout.layouts = {
    awful.layout.suit.tile,
    awful.layout.suit.max,
    awful.layout.suit.magnifier,
    awful.layout.suit.floating,
}
awful.mouse.snap.edge_enabled = false
awful.mouse.snap.client_enabled = false
-- }}}

-- {{{ Menu
-- Create a launcher widget and a main menu
myawesomemenu = {
   { "hotkeys", function() hotkeys_popup.show_help(nil, awful.screen.focused()) end },
   { "manual", terminal .. " -e man awesome" },
   { "edit config", editor_cmd .. " " .. awesome.conffile },
   { "restart", awesome.restart },
   { "quit", function() awesome.quit() end },
}

mymainmenu = awful.menu({ items = { { "awesome", myawesomemenu, beautiful.awesome_icon },
                                    { "open terminal", terminal }
                                  }
                        })

mylauncher = awful.widget.launcher({ image = beautiful.awesome_icon,
                                     menu = mymainmenu })

-- Menubar configuration
menubar.utils.terminal = terminal -- Set the terminal for applications that require it
-- }}}

-- {{{ Wibar
-- Create a textclock widget
mytextclock = wibox.widget.textclock()

-- Create a wibox for each screen and add it
local taglist_buttons = gears.table.join(
                    awful.button({ }, 1, function(t) t:view_only() end),
                    awful.button({ modkey }, 1, function(t)
                                              if client.focus then
                                                  client.focus:move_to_tag(t)
                                              end
                                          end),
                    awful.button({ }, 3, awful.tag.viewtoggle),
                    awful.button({ modkey }, 3, function(t)
                                              if client.focus then
                                                  client.focus:toggle_tag(t)
                                              end
                                          end),
                    awful.button({ }, 4, function(t) awful.tag.viewprev(t.screen) end),
                    awful.button({ }, 5, function(t) awful.tag.viewnext(t.screen) end)
                )

local tasklist_buttons = gears.table.join(
                     awful.button({ }, 1, function (c)
                                              if c == client.focus then
                                                  c.minimized = true
                                              else
                                                  c:emit_signal(
                                                      "request::activate",
                                                      "tasklist",
                                                      {raise = true}
                                                  )
                                              end
                                          end),
                     awful.button({ }, 3, function()
                                              awful.menu.client_list({ theme = { width = 250 } })
                                          end),
                     awful.button({ }, 4, function ()
                                              awful.client.focus.byidx(1)
                                          end),
                     awful.button({ }, 5, function ()
                                              awful.client.focus.byidx(-1)
                                          end))

local function set_wallpaper(s)
    -- Wallpaper
    if beautiful.wallpaper then
        local wallpaper = beautiful.wallpaper
        -- If wallpaper is a function, call it with the screen
        if type(wallpaper) == "function" then
            wallpaper = wallpaper(s)
        end
        gears.wallpaper.maximized(wallpaper, s, true)
    end
end

-- Re-set wallpaper when a screen's geometry changes (e.g. different resolution)
screen.connect_signal("property::geometry", set_wallpaper)

awful.screen.connect_for_each_screen(function(s)
    -- Wallpaper
    set_wallpaper(s)

    -- Each screen has its own tag table.
    awful.tag({ "o", "o", "o", "o", "o"}, s, awful.layout.layouts[1])

    -- Create a promptbox for each screen
    s.mypromptbox = awful.widget.prompt()
    -- Create an imagebox widget which will contain an icon indicating which layout we're using.
    -- We need one layoutbox per screen.
    s.mylayoutbox = awful.widget.layoutbox(s)
    s.mylayoutbox:buttons(gears.table.join(
                           awful.button({ }, 1, function () awful.layout.inc( 1) end),
                           awful.button({ }, 3, function () awful.layout.inc(-1) end),
                           awful.button({ }, 4, function () awful.layout.inc( 1) end),
                           awful.button({ }, 5, function () awful.layout.inc(-1) end)))
    -- Create a taglist widget
    s.mytaglist = awful.widget.taglist {
        screen  = s,
        filter  = awful.widget.taglist.filter.all,
        buttons = taglist_buttons
    }

    -- Create a tasklist widget
    s.mytasklist = awful.widget.tasklist {
        screen  = s,
        filter  = awful.widget.tasklist.filter.currenttags,
        buttons = tasklist_buttons
    }

    -- Create the wibox
    s.mywibox = awful.wibar({ position = "top", screen = s })

    -- Add widgets to the wibox
    s.mywibox:setup {
        layout = wibox.layout.align.horizontal,
        { -- Left widgets
            layout = wibox.layout.fixed.horizontal,
            -- mylauncher,
            s.mytaglist,
            s.mypromptbox,
        },
        s.mytasklist, -- Middle widget
        { -- Right widgets
            layout = wibox.layout.fixed.horizontal,
            wibox.widget.systray(),
            mytextclock,
            s.mylayoutbox,
        },
    }
end)
-- }}}

-- {{{ Mouse bindings
root.buttons(gears.table.join(
    awful.button({ }, 3, function () mymainmenu:toggle() end)
    -- awful.button({ }, 4, awful.tag.viewnext),
    -- awful.button({ }, 5, awful.tag.viewprev)
))

function double_click_event_handler(double_click_event)
    if double_click_timer then
        double_click_timer:stop()
        double_click_timer = nil
        return true
    end

    double_click_timer = gears.timer.start_new(0.20, function()
        double_click_timer = nil
        return false
    end)
end

-- }}}

-- {{{ Key bindings

-- Functions {{{

local function move_to_previous_tag()
    local c = client.focus
    if not c then return end
    local t = c.screen.selected_tag
    local tags = c.screen.tags
    local idx = t.index
    local newtag = tags[gmath.cycle(#tags, idx - 1)]
    c:move_to_tag(newtag)
    awful.tag.viewprev()
end

local function move_to_next_tag()
    local c = client.focus
    if not c then return end
    local t = c.screen.selected_tag
    local tags = c.screen.tags
    local idx = t.index
    local newtag = tags[gmath.cycle(#tags, idx + 1)]
    c:move_to_tag(newtag)
    awful.tag.viewnext()
end

local function swap_screens()
    local s = awful.screen.focused()
    local t  = s.selected_tag
    local t2 = screen[gmath.cycle(screen.instances(), s.index + 1)].selected_tag
    t:swap(t2)
    -- awful.screen.focus_relative(1)
end

local function swap_next_tag()
    local t = client.focus.screen.selected_tag
    local tags = awful.screen.focused().tags
    local nt = tags[gmath.cycle(#tags, t.index+1)]
    t:swap(nt)
    -- awful.tag.viewnext()
end

local function swap_prev_tag()
    local t = client.focus.screen.selected_tag
    local tags = awful.screen.focused().tags
    local nt = tags[gmath.cycle(#tags, t.index-1)]
    t:swap(nt)
    -- awful.tag.viewnext()
end

-- END Functions }}}

globalkeys = gears.table.join(

    -- Move across tags
    awful.key({ modkey,           }, "m",      awful.tag.viewprev),
    awful.key({ modkey,           }, "i",      awful.tag.viewnext),

    awful.key({ modkey, "Mod1"    }, "o",      function () swap_screens() end),

    awful.key({ modkey,           }, "n",      function () awful.client.focus.byidx( 1)   end),
    awful.key({ modkey,           }, "e",      function () awful.client.focus.byidx(-1)   end),
    -- Layout manipulation
    awful.key({ modkey, "Control" }, "n",      function () awful.client.swap.byidx( 1)    end),
    awful.key({ modkey, "Control" }, "e",      function () awful.client.swap.byidx(-1)    end),
    awful.key({ modkey, "Control" }, "m",      function () move_to_previous_tag()         end),
    awful.key({ modkey, "Control" }, "i",      function () move_to_next_tag()             end),
    awful.key({ modkey, "Mod1"    }, "i",      function () swap_next_tag()                end),
    awful.key({ modkey, "Mod1"    }, "m",      function () swap_prev_tag()                end),
    awful.key({ modkey,           }, "o",      function () awful.screen.focus_relative(1) end),

    -- Standard program
    awful.key({ modkey,           }, "Return", function () awful.spawn(terminal) end),
    awful.key({ modkey, "Control" }, "r",      awesome.restart),
    awful.key({ modkey, "Shift"   }, "q",      awesome.quit),
    awful.key({ modkey,           }, "Right",  function () awful.tag.incmwfact( 0.1)           end),
    awful.key({ modkey,           }, "Left",   function () awful.tag.incmwfact(-0.1)           end),
    awful.key({ modkey,           }, "Up",     function () awful.client.incwfact( 0.1)         end),
    awful.key({ modkey,           }, "Down",   function () awful.client.incwfact(-0.1)         end),
    awful.key({ modkey,           }, ",",      function () awful.tag.incnmaster( 1, nil, true) end),
    awful.key({ modkey,           }, ".",      function () awful.tag.incnmaster(-1, nil, true) end),
    awful.key({ modkey, "Control" }, ",",      function () awful.tag.incncol( 1, nil, true)    end),
    awful.key({ modkey, "Control" }, ".",      function () awful.tag.incncol(-1, nil, true)    end),
    awful.key({ modkey,           }, "d",      function () awful.layout.inc( 1)                end),


    -- Volume keys
    awful.key({}, "XF86AudioRaiseVolume", function () awful.util.spawn("amixer set Master 5%+ unmute",    false) end),
    awful.key({}, "XF86AudioLowerVolume", function () awful.util.spawn("amixer set Master 5%- unmute",    false) end),
    awful.key({}, "XF86AudioMute",        function () awful.util.spawn("amixer set Master toggle",        false) end),

    -- Media keys
    awful.key({}, "XF86AudioPlay",        function () awful.util.spawn("playerctl -p spotify play-pause", false) end),
    awful.key({}, "XF86AudioRewind",      function () awful.util.spawn("playerctl -p spotify previous",   false) end),
    awful.key({}, "XF86AudioForward",     function () awful.util.spawn("playerctl -p spotify next",       false) end),

    -- Utils
    awful.key({"Control" }, "space", function () awful.util.spawn("rofi -m -4 -combi-modi 'window,drun' -show combi -modi combi",                                     false) end),
    awful.key({ modkey,  }, "s",     function () awful.spawn.with_shell("scrot -s -f -b ~/scrot.png && xclip -selection clipboard -t image/png ~/scrot.png && rm ~/scrot.png",    false) end)
    -- awful.key({ modkey,  }, "r",     function () awful.spawn.with_shell("scrot -s -b ~/scrot.png && xclip -selection clipboard -t image/png ~/scrot.png && rm ~/scrot.png", false) end)

)

clientkeys = gears.table.join(
    awful.key({ modkey,           }, 'b', awful.titlebar.toggle),
    awful.key({ modkey,           }, "c", function (c) c:kill()                         end),
    awful.key({ modkey,           }, "f",
        function(c)
            if c.floating then
                c.floating = false
                awful.titlebar.hide(c)
            else
                c.floating = true
                awful.titlebar.show(c)
                c:geometry({width=1200, height=700})
                awful.placement.centered(c)
            end
        end),
    awful.key({ modkey, "Control" }, "l", function (c) c:swap(awful.client.getmaster()) end),
    awful.key({ modkey, "Control" }, "o", function (c) c:move_to_screen()               end),
    awful.key({ modkey,           }, "x", function (c) c.maximized = not c.maximized c:raise() end)
)


local function xdotool(combination)
    return awful.util.spawn("xdotool key --clearmodifiers" .. combination)
end

firefox_keys = gears.table.join(
    awful.key(meh, "n", function (c) awful.util.spawn("xdotool key --clearmodifiers Control+Tab") end),
    -- awful.key(meh, "n", xdotool("Control+Tab") end),
    awful.key(meh, "e", function (c) awful.util.spawn("xdotool key --clearmodifiers Control+Shift+Tab") end)
)

discord_keys = gears.table.join(
    awful.key(meh, "n", function (c) awful.util.spawn("xdotool key --clearmodifiers Alt+Down") end),
    awful.key(meh, "e", function (c) awful.util.spawn("xdotool key --clearmodifiers Alt+Up") end),

    awful.key(meh, "Down", function (c) awful.util.spawn("xdotool key --clearmodifiers Alt+Shift+Down") end),
    awful.key(meh, "Up", function (c) awful.util.spawn("xdotool key --clearmodifiers Alt+Shift+Up") end),

    awful.key(meh, "m", function (c) awful.util.spawn("xdotool key --clearmodifiers Control+Alt+Down") end),
    awful.key(meh, "i", function (c) awful.util.spawn("xdotool key --clearmodifiers Control+Alt+Up") end)
)


clientbuttons = gears.table.join(
    awful.button({ }, 1, function (c)
        c:emit_signal("request::activate", "mouse_click", {raise = true})
    end),
    awful.button({ modkey }, 1, function (c)
        c:emit_signal("request::activate", "mouse_click", {raise = true})
        awful.mouse.client.move(c)
    end),
    awful.button({ modkey }, 3, function (c)
        c:emit_signal("request::activate", "mouse_click", {raise = true})
        awful.mouse.client.resize(c)
    end)
)

-- Set keys
root.keys(globalkeys)
-- }}}

-- {{{ Rules
-- Rules to apply to new clients (through the "manage" signal).
awful.rules.rules = {
    -- All clients will match this rule.
    { rule = { },
      properties = { border_width = beautiful.border_width,
                     border_color = beautiful.border_normal,
                     focus = awful.client.focus.filter,
                     raise = true,
                     keys = clientkeys,
                     buttons = clientbuttons,
                     screen = awful.screen.preferred,
                     placement = awful.placement.no_overlap+awful.placement.no_offscreen,
                     maximized_horizontal = false,
                     maximized_vertical = false,
                     maximized = false,
     }
    },

    { rule = {class = "firefox"},
      properties = { keys = gears.table.join(clientkeys, firefox_keys),}
    },

    { rule = {class = "discord"},
      properties = { keys = gears.table.join(clientkeys, discord_keys),}
    },

    -- Floating clients.
    { rule_any = {
        instance = {
        },
        class = {
         },

        -- Note that the name property shown in xprop might be set slightly after creation of the client
        -- and the name shown there might not match defined rules here.
        name = {
          "Event Tester",  -- xev.
        },
        role = {
        }
      }, properties = { floating = true }},

    -- Add titlebars to normal clients and dialogs
    { rule_any = {type = { "normal", "dialog" }
      }, properties = { titlebars_enabled = false }
    },
    { rule_any = { name = { "plank" } }, properties = { ontop = true } },
    { rule_any = { class = { "eww" } }, properties = { border_width=0 } },
    { rule_any = { name = { "xfce4-panel" } }, properties = { ontop = true } },
    { rule_any = { name = { "menu" } }, properties = { border_width=0 } }


    -- Set Firefox to always map on the tag named "2" on screen 1.
    -- { rule = { class = "Firefox" },
    --   properties = { screen = 1, tag = "2" } },
}
-- }}}

-- {{{ Signals
-- Signal function to execute when a new client appears.
client.connect_signal("manage", function (c)


    -- Set the windows at the slave,
    -- i.e. put it at the end of others instead of setting it master.
    if not awesome.startup then awful.client.setslave(c) end

    if awesome.startup
      and not c.size_hints.user_position
      and not c.size_hints.program_position then
        -- Prevent clients from being unreachable after screen count changes.
        awful.placement.no_offscreen(c)
    end
    
end)

-- Add a titlebar if titlebars_enabled is set to true in the rules.
client.connect_signal("request::titlebars", function(c)
    -- buttons for the titlebar
    local buttons = gears.table.join(
        awful.button({ }, 1, function()
            c:emit_signal("request::activate", "titlebar", {raise = true})
             -- WILL EXECUTE THIS ON DOUBLE CLICK
                if double_click_event_handler() then
                    c.maximized = not c.maximized
                    c:raise()
                else
                    awful.mouse.client.move(c)
                end
        end),
        awful.button({ }, 3, function()
            c:emit_signal("request::activate", "titlebar", {raise = true})
            awful.mouse.client.resize(c)
        end)
    )

    awful.titlebar(c) : setup {
        { -- Left
            awful.titlebar.widget.iconwidget(c),
            buttons = buttons,
            layout  = wibox.layout.fixed.horizontal
        },
        { -- Middle
            { -- Title
                align  = "center",
                widget = awful.titlebar.widget.titlewidget(c)
            },
            buttons = buttons,
            layout  = wibox.layout.flex.horizontal
        },
        { -- Right
            awful.titlebar.widget.floatingbutton (c),
            awful.titlebar.widget.stickybutton   (c),
            awful.titlebar.widget.ontopbutton    (c),
            awful.titlebar.widget.maximizedbutton(c),
            awful.titlebar.widget.closebutton    (c),
            layout = wibox.layout.fixed.horizontal()
        },
        layout = wibox.layout.align.horizontal
    }
end)

-- Enable sloppy focus, so that focus follows mouse.
-- client.connect_signal("mouse::enter", function(c)
    -- c:emit_signal("request::activate", "mouse_enter", {raise = false})
-- end)

client.connect_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)
-- }}}
