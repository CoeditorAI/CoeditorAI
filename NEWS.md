# CoeditorAI 1.1.0

The `CoeditorAI` add-in is now compatible with Positron IDE! A new, generous `freemium` plan is available for all users. We cover the costs, and you benefit!

## Breaking changes:

* Renamed the GitHub repository from `CoeditorAI/CoeditorAI-RStudio` to `CoeditorAI/CoeditorAI` (without the suffix) to highlight that our add-in now functions with both the RStudio and Positron IDEs.

## Major changes:

* Integrated support for the Positron IDE.  
* Updated the LLM model to the high-performing OpenAI `GPT-4o` model.  
* Modified the `free-trial` plan to the `freemium-user` plan.  
The new freemium plan is available to all users at no cost!  
However, it limits the number of daily requests to the `CoeditorAI` API.  
The current daily cap is 100 requests.  

## Minor changes:

* Enhanced UX by allowing modal dialogs to be closed with the `Enter` key (when using `Translate` or `Custom prompt`).
* Improved prompts for `rewrite`, `proofread`, and `translate` API endpoints.
* `Translate` now retains the last target language set within the R session.
* `Custom prompt` now retains the last custom prompt configured in the R session.
The text area is wider and resizable. The custom prompt can be edited each time and will be remembered within the R session. It can be set at the start of the R session using `options(coeditorai.custom_prompt = "Return only first word.")`.


# CoeditorAI 1.0.0

## Major changes:

* First version of `CoeditorAI RStudio Addin` and `CoeditorAI API`.
* Added first AI-based core co-editing features like: `Rewrite`, `Proofread`,
`Translate`, and `Custom prompt`.
* Added free-trial subscription plan for new users.
* Added CoeditorAI home page: https://coeditorai.com/
