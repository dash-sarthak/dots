config.load_autoconfig()

c.aliases = {'q': 'quit', 'w': 'session-save', 'wq': 'quit --save'}

# Get Rid of Auto Load Dialogue
config.load_autoconfig(False)

config.set('content.cookies.accept', 'all', 'chrome-devtools://*')
config.set('content.cookies.accept', 'all', 'devtools://*')

# User agent to send.
config.set('content.headers.user_agent', 'Mozilla/5.0 ({os_info}) AppleWebKit/{webkit_version} (KHTML, like Gecko) {upstream_browser_key}/{upstream_browser_version} Safari/{webkit_version}', 'https://web.whatsapp.com/')
config.set('content.headers.user_agent', 'Mozilla/5.0 ({os_info}; rv:71.0) Gecko/20100101 Firefox/71.0', 'https://accounts.google.com/*')
config.set('content.headers.user_agent', 'Mozilla/5.0 ({os_info}) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/99 Safari/537.36', 'https://*.slack.com/*')
config.set('content.headers.user_agent', 'Mozilla/5.0 ({os_info}; rv:71.0) Gecko/20100101 Firefox/71.0', 'https://docs.google.com/*')
config.set('content.headers.user_agent', 'Mozilla/5.0 ({os_info}; rv:71.0) Gecko/20100101 Firefox/71.0', 'https://drive.google.com/*')

# Load images automatically in web pages.
config.set('content.images', True, 'chrome-devtools://*')
config.set('content.images', True, 'devtools://*')

# Enable JavaScript.
config.set('content.javascript.enabled', True, 'chrome-devtools://*')
config.set('content.javascript.enabled', True, 'devtools://*')
config.set('content.javascript.enabled', True, 'chrome://*/*')
config.set('content.javascript.enabled', True, 'qute://*/*')

c.tabs.show = 'multiple'

# Setting default page for when opening new tabs or new windows with
# commands like :open -t and :open -w .
c.url.default_page = 'https://search.brave.com'

c.url.searchengines = {'DEFAULT': 'https://search.brave.com/search?q={}'}

c.downloads.position = 'bottom'

c.fonts.statusbar = '12pt JetBrainsMono Nerd Font'
c.fonts.prompts = '12pt JetBrainsMono Nerd Font'
c.fonts.tabs.selected = '12pt JetBrainsMono Nerd Font'
c.fonts.tabs.unselected = '12pt JetBrainsMono Nerd Font'

c.search.incremental = True
c.editor.command = ['kitty', '-e', 'nvim', '{}']

c.colors.webpage.darkmode.enabled = True

config.source('gruvbox.py')
