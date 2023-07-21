# Mail Sending Testing

To test mail sending, you need to download the MailHog file corresponding to your operating system and architecture from [this repository](https://github.com/mailhog/MailHog/blob/master/docs/RELEASES.md). For example: _MailHog_windows_386.exe_.

Once downloaded, execute the MailHog file to test the email sending, following the documentation provided in the repository. You will see two IP addresses displayed in the console, each corresponding to a different port.

![image](https://github.com/nicovillamonte/lapsa-frontend/assets/64659720/cbfb1e62-e643-45f5-b255-1cc1b627d3fc)

The port numbered as 1 is the one you need to configure in the backend you are developing. Here's an example developed in _NestJS_ for configuring the main module of the project:

```typescript
MailerModule.forRoot({
  transport: {
    host: '0.0.0.0',
    port: 1025,
  },
  defaults: {
    from: 'admin@lapsa.com',
  },
}),
```

On the other hand, the port number 2 is the one you should use to access the UI where you can view the sent mails.

In this case, if the port is **8025**, you need to navigate to **http://localhost:8025**. There, you will find a page like the following:

![image](https://github.com/nicovillamonte/lapsa-frontend/assets/64659720/b7a1ccb2-64ab-416d-99ef-bad92a37e80c)

When you send an email with the correct configuration from the backend, you can view it in the following way:

![image](https://github.com/nicovillamonte/lapsa-frontend/assets/64659720/79336a68-fc8a-484f-8e25-dfd72fe343a5)

Upon opening the email, you can see it in various formats. The _HTML_ format is what the user would receive. The _Plain Text_ format shows the HTML code that represents the message. And the source contains more detailed information about the email sending.

![image](https://github.com/nicovillamonte/lapsa-frontend/assets/64659720/f510c229-195a-4667-85f3-46a184c62835)
