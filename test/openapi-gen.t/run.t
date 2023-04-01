  $ oooapi
  module OpenAI_api =
    struct
      let start_chat = Client.request "chat" "mydata"
      let end_chat = Client.request "chat/end" "foo"
      let completion = Client.request "complete" "mycompletiondata"
    end
