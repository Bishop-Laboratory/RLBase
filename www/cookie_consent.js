window.cookieconsent.initialise({
    container: document.getElementById("cookieconsent"),
    palette:{
     popup: {background: "#1E6D83"},
     button: {background: "#e0e0e0"},
    },
    revokable: true,
    onStatusChange: function(status) {
     console.log(this.hasConsented() ?
      'enable cookies' : 'disable cookies');
    },
    "position": "bottom-right",
    "theme": "classic",
    "domain": "https://i71n4.csb.app/",
    "secure": true,
    "content": {
      "header": 'Cookies used on the website!',
      "message": 'This website uses cookies to improve your experience.',
      "dismiss": 'Got it!',
      "allow": 'Allow cookies',
      "deny": 'Decline',
      "link": 'Learn more',
      "href": 'https://www.cookiesandyou.com',
      "close": '&#x274c;',
      "policy": 'Cookie Policy',
      "target": '_blank',
    }
});
