# wwdcc

`wwdcc` is a service for monitoring the Apple WWDC site for changes.
When a change is detected, or when the site stops responding, `wwdcc`
sends one or more notifications to specified email addresses and/or
phone numbers via SMS.

Note that `wwdcc` is extremely stupid: it simply scrapes the HTML
served by the WWDC site and looks for a "canary" string. If that
canary string isn't found, or if it isn't surrounded by the expected
tags, `wwdcc` will send its change notifications. As of the current
version, the canary string that `wwdcc` looks for is *"WWDC 2012. June
11-15 in San Francisco. It's the week we've all been waiting for."*

Unless it is terminated, either by the user or due to a bug or some
kind of system problem, `wwdcc` will run until it detects a change, at
which point it will exit after sending its change notifications.

## Notification types

`wwdcc` supports two types of notification.

### Email<a id="Email"></a>

If your system has a working `sendmail` configuration, `wwdcc` can
send SMTP email to a destination address. If you tell `wwdcc` to send
email notifications, it will attempt to send an activation
notification when it starts. Often, if your system's `sendmail` is not
configured properly, this will trigger an exception, and `wwdcc` will
quit with an error message. However, some `sendmail` configurations
may accept the email but not deliver it properly. Debugging `sendmail`
configurations is outside the scope of this document, but [Google is
your friend](http://www.google.com/?q=how+to+configure+sendmail).

You will probably want to train your spam catcher to accept emails
from `wwdcc`. You can do this either by whitelisting the `From:`
address that you specify to `wwdcc`, or by marking the activation
email sent by `wwdcc` when it starts as "ham."

In any case, if you start `wwdcc` with email notifications, and you
don't see an activation email from it, check your spam folder.

### SMS (via Twilio)

If you have a Twilio developer account, `wwdcc` can use your Twilio
developer credentials to send SMS notifications. Just put your Twilio
account SID and auth token in your `~/.wwdcc` config file, like so:

<pre>
twilio {
  accountSid = "Your Twilio Account SID here"
  authToken = "The corresponding Twilio auth token here"
}
</pre>

Then use the `--sms from_number,to_number` option, where `from_number`
is a valid Twilio phone number for your account. As with [email
notifications](#Email), `wwdcc` will attempt to send an activation SMS
at start-up, and will exit immediately if it fails. If you've
configured `wwdcc` properly and you have sufficient credits, you
should receive an activation SMS at `to_number` just a few seconds
after you launch it.

Note that if you don't have a paid Twilio account, you can only send
SMSes to the phone number you have on file with them; if you want to
send SMSes to other numbers, you'll need a paid account. Also, free
Twilio accounts have limits on the number of SMSes that can be sent.
It's probably best to use `wwdcc` with a paid Twilio account; it has
not been tested extensively with the free service.

## Notification events

`wwdcc` sends notifications in the following cases:

- When a change is detected, `wwdcc` will send a series of
  notifications (3, by default), with a delay between each (30s, by
  default). Once the last notification has been sent, it will exit.

- When the site is down, `wwdcc` will send a single notification
  indicating that the site is down if it does not respond to `wwdcc`s
  HTTP requests twice in a row. (By default, this notification is sent
  after 60s of downtime.) Note that `wwdcc` will *not* continue to
  send notifications while the site is down, nor will it send a
  notification when it recovers, unless `wwdcc` detects that the site
  has changed when it comes back up.

- When started, `wwdcc` will send a single notification indicating
  that it has started monitoring the site. If you don't receive this
  notification, make sure `wwdcc` is still running and examine its
  logs for hints of what might have gone wrong. (The `--verbose` flag
  may be useful.)

- When terminated: if `wwdcc` is terminated, either by the user or due
  to some kind of unexpected error, `wwdcc` will attempt to send a
  single notification indicating that it's been terminated. It does so
  primarily to ensure that you're aware that it's no longer monitoring
  the site. The termination notification generally does not indicate
  what went wrong, but the log file should.

- In response to a USR1 signal: if you send `wwdcc` the USR1 signal
  (e.g., via `kill -USR1 <pid>`), `wwdcc` will send an
  "I'm-still-alive" notification. This feature is useful if you have
  OCD.

Note that only when a change is detected, or when it is terminated,
will `wwdcc` quit after sending a notification. In all other cases, it
will continue to monitor the site.

## Running

<pre>
% wwdcc --help
wwdcc - a WWDC checker

Usage: wwdcc [-v|--verbose] [-c|--config PATH] [-s|--syslog] [--daemon] [-u|--url URL] [-p|--period DELAY] [-n|--notifications NUM] [-w|--wait DELAY] [-s|--sms FROM_NUMBER,TO_NUMBER] [-e|--email FROM_EMAIL,TO_EMAIL]
  Send a notification(s) when WWDC site changes or stops responding.

Available options:
  -h,--help                Show this help text
  -v,--verbose             Verbose logging
  -c,--config PATH         Path to config file (default: "$(HOME)/.wwdcc")
  -s,--syslog              Log to syslog (default: log to stderr)
  --daemon                 Run as a daemon (implies --syslog)
  -u,--url URL             Override WWDC URL (default: "https://developer.apple.com/wwdc/")
  -p,--period DELAY        Time between pings, in seconds (default: 30)
  -n,--notifications NUM   Number of notifications to send when a change is detected (default: 3)
  -w,--wait DELAY          Time between notifications, in seconds (default: 30)
  -s,--sms FROM_NUMBER,TO_NUMBER Send SMS notifications from/to phone number, comma-delimited (default: don't send SMS notifications).
  -e,--email FROM_EMAIL,TO_EMAIL Send email notifications from/to address, comma-delimited (default: don't send email notifications).
</pre>

## License

`wwdcc` is published under the [BSD
3-Clause](http://opensource.org/licenses/BSD-3-Clause) license.

## Contact

For comments or questions, please [contact
us](mailto:src@quixoftic.com).
